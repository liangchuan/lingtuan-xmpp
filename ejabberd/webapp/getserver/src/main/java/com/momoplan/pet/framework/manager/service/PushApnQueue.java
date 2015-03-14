package com.momoplan.pet.framework.manager.service;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import javax.annotation.PostConstruct;

import javapns.json.JSONObject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.momoplan.pet.framework.manager.node.apnspush.ApnsPushLog;
/**
 * 
 * @author liangc
 *	2015-03-12 modify by liangc :  
 *		戚银 111 之前的群聊变更名称为“讨论组”，提示消息涉及到群聊文字的统一改为讨论组（如，xxx加入群聊，改成xxx加入讨论组）
 */
@Component
public class PushApnQueue {
	
	private static Logger apns_logger = LoggerFactory.getLogger(ApnsPushLog.class);
	
	private static BlockingQueue<OtpErlangObject> q = new ArrayBlockingQueue<OtpErlangObject>(300000);
	
	public void push(OtpErlangObject obj){
		try {
			PushApnQueue.q.put(obj);
		} catch (InterruptedException e) {
			apns_logger.error("push_queue_error",e);
			e.printStackTrace();
		}
	}
	
	@PostConstruct
	public void init(){
		for(int i=1000;i<3000;i++){
			Thread t = new Thread(new Runnable(){
				@Override
				public void run() {
					while(true){
						try {
							OtpErlangObject obj = q.take();
							String name = Thread.currentThread().getName();
							int size = q.size();
							apns_logger.debug("current_thread_name="+name+" ; queue_size="+size);
							apns_push(obj);
						} catch (InterruptedException e) {
							apns_logger.error("push_thread_error",e);
							e.printStackTrace();
						} catch (Exception e) {
							apns_logger.error("push_thread_error",e);
							e.printStackTrace();
						}
					}
				}
			});
			t.setName("push_apn_thread_"+i);
			t.start();
		}
	}
	
	private void apns_push(OtpErlangObject obj) {
		try {
			// tuple ::> {apns_push:atom,id:string,from:string,to:string,msgtype:string,msg:binary,badge:integer}
			OtpErlangTuple tuple = (OtpErlangTuple) obj;
			apns_logger.debug("tuple == "+tuple.toString());
			OtpErlangAtom flag = (OtpErlangAtom) tuple.elementAt(0);
			if ("apns_push".equalsIgnoreCase(flag.atomValue())) {
				String id = ((OtpErlangString) tuple.elementAt(1)).stringValue();
				String from = ((OtpErlangString) tuple.elementAt(2)).stringValue();
				String to = ((OtpErlangString) tuple.elementAt(3)).stringValue();
				String msgtype = ((OtpErlangString) tuple.elementAt(4)).stringValue();
				OtpErlangBinary bin = ((OtpErlangBinary) tuple.elementAt(5));
				String msg = new String(bin.binaryValue());

				int badge = 0;
				try{
					apns_logger.debug("has_badge id="+id);
					String badgeObj = ((OtpErlangString) tuple.elementAt(6)).stringValue();
					apns_logger.debug("has_badge id="+id+" ; badgeObj="+badgeObj);
					badge = Integer.parseInt(badgeObj);
					apns_logger.debug("has_badge id="+id+" ; badge="+badge);
				}catch(Exception e){
					apns_logger.error("has_badge id="+id+" ; error_msg="+e.getMessage());
					e.printStackTrace();
					apns_logger.error("badge", e);
				}finally{
					//如果是0，说明消息不在队列里，但一定会有消息，这里应该给一个默认值1
					if(badge<1){
						apns_logger.debug("badge_empty default 1");			
						badge = 1;
					}
				}
				apns_logger.debug("badge_result_is "+badge);			

				StringBuffer log = new StringBuffer("\02");
				log.append(id).append("\01");
				log.append(from).append("\01");
				log.append(to).append("\01");
				log.append(msgtype).append("\01");
				log.append(msg).append("\01");
				log.append(badge).append("\01");
				apns_logger.debug(log.toString());
				try{
					JSONObject json = new JSONObject();
					JSONObject params = new JSONObject();
					params.put("username", to.split("@")[0]);
					json.put("service", "ejabberd");
					json.put("method", "getdevicetoken");
					json.put("sn", id);
					json.put("params",params);
					String args = json.toString();
					//{"sn":"700c8569-5c78-44e8-b082-7e16c31ef773","success":true,"devicetoken":""}
					String url = "http://open.yuenimei.com.cn/index.php";
					if(Config.publicConfig.get("apns_push_url")!=null){
						url = Config.publicConfig.get("apns_push_url");
					}
					apns_logger.debug(id+"::>url="+url+" ; args="+args);
					String res = PostRequest.postText(url, "body",args);
					apns_logger.info(id+"::>callback: request="+args+" ; response="+res);
					JSONObject result = new JSONObject(res);
					String resmsg = build_push_msg(msgtype,msg);
					boolean with_token = false;
					if(resmsg!=null && resmsg.split("\01").length>1){
						with_token = true;
					}
					if( with_token || result.getBoolean("success") ){
						String deviceToken = null;
						String cert = null;
						Map<String,String> map = new HashMap<String,String>();
						map.put("msgtype", msgtype);
						apns_logger.debug(id+"::>push="+msg+" ; map="+map);
						try{
							if(resmsg!=null){
								String[] resmsgArr = resmsg.split("\01");
								if(with_token){
									apns_logger.info("with_token__devicetoken=="+resmsgArr[0]);
									deviceToken = resmsgArr[0];
									resmsg = resmsgArr[1];
								}else{
									deviceToken = result.getString("devicetoken");
								}
								cert = result.getString("cert");
								if(deviceToken!=null&&!"".equals(deviceToken)){
									//TODO badge
									//String apns_push_pwd = Config.publicConfig.get("apns_push_pwd");
									String apns_push_pwd = Config.publicConfig.get(cert);
									
									apns_logger.debug("[debug_push] badge="+badge+" ; "+id+"::>push="+msg+" ; map="+map+" ; resmsg="+resmsg+" ; apns_push_pwd="+apns_push_pwd);
									PushApn.push(deviceToken, resmsg, apns_push_pwd, isDebug(id), map,badge,cert);
								}else{
									apns_logger.debug("id="+id+" ::> devicetoken_is_null");
								}
							}else{
								apns_logger.debug(id+"::>push="+msg+" ; map="+map+ " ; resmsg="+resmsg);
							}
						}catch(Exception err){
							apns_logger.error(id,err);
							err.printStackTrace();
						}
					}
				}catch(Exception e){
					apns_logger.error("push_error",e);
				}
			}
		} catch (Exception e) {
			apns_logger.debug("apns_error", e);
		}
	}
	
	private boolean isDebug(String id){
		boolean debug = false;
		apns_logger.debug(id+"::>debug="+debug);
		try{
			debug = Boolean.parseBoolean(Config.publicConfig.get("apns_push_debug"));
		}catch(Exception e){
			e.printStackTrace();
		}
		return debug;
	}
	
	private String build_push_msg(String msgtype ,String jmsg) throws Exception{
		JSONObject json = new JSONObject(jmsg);
		String type = json.getString("type");
		//0 未屏蔽  1屏蔽   如下所示：
		String mask = "0";
		if(json.has("mask")){
			mask = json.getString("mask");
		}
		if("normalchat".equalsIgnoreCase(msgtype) && (!"1".equals(mask)) ){
//			1.单人聊天信息:         
			if("0".equals(type)){
//			 <1>文本消息： ------------------logo+约您妹  用户昵称：最新聊天的语言（最多显示61个汉字）
//		     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//		     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
//		     </body>
//		     </message>
				String username = json.getString("username");
				String content = json.getString("content");
				if(content!=null && content.length()>40){
					content = content.substring(0, 40)+"...";
				}
				return username+":"+content;
			}else if("1".equals(type)){
//				  <2>图片消息：缩略图=cover------------------logo+约您妹  用户昵称发来一张图片
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"1","content":"url","cover":"base64编码图片"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来一张图片";
			}else if("2".equals(type)){
//				  <3>语音消息：amr格式------------------logo+约您妹  用户昵称发来一段语音
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"2","content":"url","second":"60"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来一段语音";
//			}else if("3".equals(type)){
//				  <4>gif动画消息：------------------logo+约您妹  用户昵称：gift识别出来的文字（暂无）
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"3","content":"url"}
//			     </body>
//			     </message>
				
			}else if("4".equals(type)){
//				  <5>位置消息：------------------  logo+约您妹  用户昵称：【我的位置】
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"4","lon":"39.000","lat":"139.000"}</body> 高德坐标
//			     </message>
				String username = json.getString("username");
				return username+"【我的位置】";
			}else if("5".equals(type)){
//				  <6>场景消息：------------------  logo+约您妹  用户昵称发来了约会场景
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"5","name":"商家名称","address":"商家地址","image":"url","id":"场景id，产品详情页的id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来了约会场景";
			}else if("6".equals(type)){
//				<7>名片消息：------------------  logo+约您妹  用户昵称发来了XXX的名片  
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"6","name":"名片人的用户名","image":"url","id":"用户id",“sign”:"签名"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				String name = json.getString("name");
				return username+"发来了"+name+"的名片";
			}else if("7".equals(type)){
				//add by liangc 140824
//				<8>求约消息：           apns推送内容（用户昵称发来了求约 ）
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"7","name":"用户名","image":"url","id":"用户id",“content”:"求约内容"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来了求约";
			}else if("8".equals(type)){
				//add by liangc 140824
//				<9>同意求约消息：           apns推送内容（用户昵称同意了您的求约 ）
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"8","name":"用户名","image":"url","id":"用户id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"同意了您的求约";
			}else if("9".equals(type)){
				//add by liangc 140824
//				<10>拒绝求约消息：           apns推送内容（用户昵称拒绝了您的求约 ）
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"9","name":"用户名","image":"url","id":"用户id"}
//			     </body>
//			     </message>			
				String username = json.getString("username");
				return username+"拒绝了您的求约";
			}else if("10".equals(type)){
				//add by liangc 140924
				//<11>帮问消息：           apns推送内容（用户昵称想帮您介绍一个朋友 ）   msgtype  0 问帮约对象  1问邀请对象
				// <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
				//<body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"10","name":"用户名","gender":"1","image":"url","id":"用户id",“content”:"帮问内容",",'msgtype":"0"}
				//</body>
				//</message>
				String username = json.getString("username");
				return username+"想帮您介绍一个朋友";
			}else if("11".equals(type)){
				//add by liangc 140924
				//<12>帮问的同意消息：    apns推送内容（用户昵称同意了您的介绍）   msgtype  0 帮约对象回复  1邀请对象回复
				// <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
				//<body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"11","name":"用户名","gender":"1","image":"url","id":"用户id",“content”:"帮问内容",",'msgtype":"0"}
				//</body>
				//</message>
				String username = json.getString("username");
				return username+"同意了您的介绍";
			}else if("12".equals(type)){
				//add by liangc 140924
				//<13>帮问的拒绝消息：    apns推送内容（抱歉，用户昵称暂时不想认识这个朋友，可能是机缘尚未成熟 ）   msgtype  0 帮约对象回复  1邀请对象回复
				// <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
				//<body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"12","name":"用户名","gender":"1","image":"url","id":"用户id",“content”:"帮问内容",",'msgtype":"0"}
				//</body>
				//</message>	
				String username = json.getString("username");
				return "抱歉，"+username+"暂时不想认识这个朋友，可能是机缘尚未成熟";
			}else if("13".equals(type)){
				//add by liangc 150126
//				单人消息<14>发送分享消息：    apns推送内容（用户昵称分享了一条链接）  friend_log 相对关系（0：陌生人；1：好友；2：二度好友）             mask  0 未屏蔽  1屏蔽  
//				 <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//				 <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"13","url":"url地址","title":"标题"."content":"内容","thumb":"图片地址","friend_log”:“0”,"mask":"0"}
//				 </body>
//				 </message>				
				String username = json.getString("username");
				return username+"分享了一条链接";
			}
			
			
			
		}else if("groupchat".equalsIgnoreCase(msgtype)){
//			2.多人会话聊天信息：groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）
			if("0".equals(type)){
//				  <1>文本消息：------------------  logo+约您妹   用户昵称：最新聊天的语言（最多显示61个汉字）
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}</body>
//			     </message>
				String username = json.getString("username");
				String content = json.getString("content");
				if(content!=null && content.length()>40){
					content = content.substring(0, 40)+"...";
				}
				return username+":"+content;
			}else if("1".equals(type)){
//				  <2>图片消息：------------------logo+约您妹  用户昵称发来一张图片
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"1","content":"url","cover":"base64编码图片"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来一张图片";
			}else if("2".equals(type)){
//				  <3>语音消息：------------------logo+约您妹  用户昵称发来一段语音
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"2","content":"url","second":"60"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来一段语音";
//			}else if("3".equals(type)){
//				  <4>gif动画消息：------------------logo+约您妹  用户昵称：gift识别出来的文字
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"3","content":"url"}
//			     </body>
//			     </message>
			}else if("4".equals(type)){
//				  <5>位置消息：------------------  logo+约您妹  用户昵称：【我的位置】
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"4","lon":"39.000","lat":"139.000"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"【我的位置】";
			}else if("5".equals(type)){
//				  <6>场景消息：------------------  logo+约您妹  用户昵称发来了约会场景
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"5","name":"商家名称","address":"商家地址","image":"url","id":"场景id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来了约会场景";
			}else if("6".equals(type)){
//				<7>名片消息：------------------  logo+约您妹  用户昵称发来了XXX的名片
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"6","name":"名片人的用户名","image":"url","id":"用户id",“sign”:"签名"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				String name = json.getString("name");
				return username+"发来了"+name+"的名片";
			}else if("13".equals(type)){
				//add by liangc 150126
//				多人消息<14>发送分享消息： 
//				<message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“groupchat”>
//				<body>{"mask":"0","groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"13","url":"url地址","title":"标题"."content":"内容","thumb":"图片地址","friend_log”:“0”,"mask":"0"}
//					</body>
//				</message>
				String username = json.getString("username");
				return username+"分享了一条链接";
			}
			
		}else if("system".equalsIgnoreCase(msgtype)){
//			（3）.约您妹系统消息定义?
			if("-1".equals(type)){
				//			1.系统消息:------------------  logo+约您妹  该账号已经在xxx机子上登陆，您被强迫下线
//			     <0>下线消息（ejabber不处理)
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com/IMEI" type="normal"  msgtype=“system”>
//			     <body>{"userid":"system","type":"-1","content":"该账号已经在xxx机子上登陆，您被强迫下线"}
//			     </body>
//			     </message>
				// XXX 2014-6-24 : 宇庭要求这个地方不处理，token有错
				//String content = json.getString("content");
				//return content;
				//XXX 2014年06月25日 18:50 宇庭
//				1.强制线下消息推送，type=-1：
//						 <message id="xxxxx" from="xx@test.com" to="yy@test.com/IMEI" type="normal"  msgtype=“system”>
//						     <body>{"userid":"system","type":"-1","content":"该账号已经在xxx机子上登陆，您被强迫下线","devicetoken":"11111111"}
//						     </body>
//						     </message>
//				这个是新定义消息的格式，增加devicetoken字段，您做apns推送这个消息的时候，
//				不要去通过web端提供的获取devkcetoken的接口取做apns推送，而直接用传递过来的devicetoken值，
//				因为您如果去调用web接口获取devicetoken，那这个devicetoken值是新登陆用户的手机的devicetoken值，
//				而不是发送给需要下线用户的手机的devicetoken值。其他消息还是通过web端提供的接口获取devicetoken值。
				String devicetoken = json.getString("devicetoken");
				String content = json.getString("content");
				apns_logger.info("__type=-1, 强制下线: "+json.toString());
				return devicetoken+"\01"+content;
			}else if("0".equals(type)){
//			     <1>好友请求消息（ejabber不处理)------------------  logo+约您妹  xxx请求您加为好友
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0"}
//			     </body>
//			     </message>
				//2014-10-15 : modify by liangc : 宇庭说这个地方取消推送
				//这个消息原先针对ios是做过apns推送的，现在需要把这个apns推送去掉，type=0的system消息不坐apns推送。
//				String username = json.getString("username");
//				return username+"请求您加为好友";
				return null;
			}else if("1".equals(type)){
//			     <2>好友请求确认消息（ejabber不处理)------------------  logo+约您妹  xxx已经通过了您的好友请求
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"1"}
//			     </body>
//			     </message>
				//String username = json.getString("username");
				//return username+"已经通过了您的好友请求";
				// 2014-10-31 : 于庭要求停止这个类型的推送
				return null;
			}else if("2".equals(type)){
//			     <3>好友删除消息（ejabber不处理)------------------  不提示
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"2"}
//			     </body>
//			     </message>
			}else if("3".equals(type)){
//			     <4>邀约请求消息（ejabber需要处理)------------------  logo+约您妹  xxx发来了邀约请求
//			    <message id="xxxxx" from="xx@test.com" to="0@group.test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0",
//			  "type":"3","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",
//			  “createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语",
//			  "address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id","toinvitedlist":[”123456","123456","123456"]}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来了邀约请求";
			}else if("4".equals(type)){
//			     <5>同意邀约请求消息（ejabber不做处理）------------------  logo+约您妹  xxx同意了您的邀约请求
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"4","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"同意了您的邀约请求";
			}else if("5".equals(type)){
//			     <6>拒绝邀约请求消息（ejabber不做处理）------------------  logo+约您妹  xxx拒绝了您的邀约请求，赶快求个安慰
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"5","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"拒绝了您的邀约请求，赶快求个安慰";
			}else if("6".equals(type)){
//			     <7>报名活动消息（ejabber不做处理）------------------  logo+约您妹  xxx报名了您发起的活动
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"6","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"报名了您发起的活动";
			}else if("7".equals(type)){
//			      <8>活动管理者 同意报名用户参加消息（ejabber需要处理）------------------  logo+约您妹  xxx通过了您报名的活动
//			     <message id="xxxxx" from="xx@test.com" to="0@group.test.com type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"7","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id","applylist":[”123456","123456","123456"]}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"通过了您报名的活动";
			}else if("8".equals(type)){
//			     <9>活动管理者 拒绝报名用户参加请求消息（ejabber需要处理）------------------  logo+约您妹  xxx拒绝了您报名的活动
//			     http发送消息格式
//			     <message id="xxxxx" from="xx@test.com" to="0@group.test.com type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"8","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id","applylist":[”123456","123456","123456"]}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"拒绝了您报名的活动";
			}else if("9".equals(type)){
//			     <10>肋友（是好友）牵线邀请消息（ejabber不做处理）------------------  logo+约您妹  有人学雷锋为您牵线发起邀约活动
//                ------------------  logo+约您妹  XXX学雷锋为您牵线发起邀约活动
//<message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//<body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"9","sort":"种类（吃喝玩乐）","guest":"1","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//</body>
//</message>
				String username = json.getString("username");
				return username+"学雷锋为您牵线发起邀约活动";
			}else if("10".equals(type)){
//			     <11>同意肋友（是好友）牵线邀请消息（ejabber不做处理）
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"10","sort":"种类（吃喝玩乐）","guest":"1","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"同意了您的牵线邀约";
			}else if("11".equals(type)){
//			    <12>拒绝肋友（是好友）牵线邀请消息（ejabber不做处理）------------------  logo+约您妹  xxx拒绝了您的牵线邀约
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"11","sort":"种类（吃喝玩乐）","guest":"1","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"拒绝了您的牵线邀约";
			}else if("12".equals(type)){
//			     <13>创建加入多人会话消息（ejabber不需要处理）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）                   ------------------  logo+约您妹  xxx创建了多人对话（消息分发给每一个讨论组人员）
//			     http发送消息格式
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"12"}</body>
//			     </message>
				//凡2014-06-25 15:26:18
				//修改下这两个传递的内容 您被邀请您加入xxxx（xxxx为多人对话名称）
				String groupname = json.getString("groupname");
				
				//戚银 2015-02-09 
//				<13>创建加入多人会话消息（ejabber不需要处理）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）          
//				apns推送内容（你被xxx邀请加入群聊）       新增参数说明：  invitename   邀请人的用户名
//			     http发送消息格式
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"12","invitename":"张三"}</body>
//			     </message>
				if(json.has("invitename")){
					String invitename = json.getString("invitename");
					//return "您被"+invitename+"邀请加入群聊";
					return "您被"+invitename+"邀请加入讨论组";
				}else{
					return "您被邀请加入"+groupname;
				}
			}else if("13".equals(type)){
//			     <14>邀请某些人加入多人会话消息（ejabber需要处理）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）------------------  logo+约您妹  xxx邀请您加入xxxx（xxxx为群组名称）
//			     http发送消息格式
//			     <message id="xxxxx" from="xx@test.com" to"yy@group.test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"13","grouplist":[”123456","123456","123456"]}</body>
//			     </message>
				//凡2014-06-25 15:26:18
				//修改下这两个传递的内容 您被邀请您加入xxxx（xxxx为多人对话名称）
				String groupname = json.getString("groupname");
				//戚银 2015-02-09 
//				 <14>邀请某些人加入多人会话消息（ejabber需要处理）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）      apns推送内容（你被xxx邀请加入群聊）   新增参数说明：  invitename   邀请人的用户名
//			     http发送消息格式
//			     <message id="xxxxx" from="xx@test.com" to"yy@group.test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"13","invitename":"张三","grouplist":[”123456","123456","123456"]}</body>
//			     </message>
//			     ejabber服务器需要给grouplist列表发送的消息格式（ejabber转发时去掉grouplist）
//			     <message id="xxxxx" from="yy@group.test.com" to"123456@test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"13","invitename":"张三"}</body>
//			     </message>
				if(json.has("invitename")){
					String invitename = json.getString("invitename");
					//return "您被"+invitename+"邀请加入群聊";
					return "您被"+invitename+"邀请加入讨论组";
				}else{
					return "您被邀请加入"+groupname;
				}
			}else if("14".equals(type)){
//			     <15>踢出某个多人会话成员消息（ejabber不需要处理，这里特别注意的是自己退出多人对话（非管理员），不发送xmpp消息）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）------------------ logo+约您妹  您被管理员踢出xxxx（xxxx为群组名称）
//			     <message id="xxxxx" from="xx@test.com" to"yy@test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"14"}</body>
//			     </message>
				String groupname = json.getString("groupname");
				return "您被管理员踢出"+groupname;
			}else if("15".equals(type)){
//			     <16>解散自己创建的多人会话消息（ejabber需要处理）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）
//			     http发送消息格式------------------ logo+约您妹  xxxx已经解散（xxxx为群组名称）
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"15","grouplist":[”123456","123456","123456"]}</body>
//			     </message>
				String groupname = json.getString("groupname");
				return groupname+"已经解散";
			}else if("16".equals(type)){
//			      <17>活动提醒消息（ejabber不做处理）------------------ logo+约您妹  离活动开始还有xxx分钟，赶快准备赴约吧
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"system","type":"16","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id","property":"公开","price":"200",“detail”:"商家描述","image":"商家图片url","invitedlist":[{"image":"邀请用户头像url","id":"123456","gender":"1"},{"image":"邀请用户头像url","id":"123456","gender":"1"}]}
//			     </body>
//			     </message>
			}else if("21".equals(type)){
				//add by liangc 140925
				//<22>邀约活动修改提醒消息：  字段说明   modifytype   0  修改了时间  1修改了地点        apns推送内容（您参与的邀约活动有所变动，请查看详情）   
				//http发送消息格式
				//<message id="xxxxx" from="xx@test.com" to="0@group.test.com type="normal" msgtype=“system”>
				//<body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg&quot;,  "usergender":"0",  "type":"21", "sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg&quot;, “createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id","modifytype":"0","applylist":[”123456","123456","123456"]}
				//</body>
				//</message>
				return "您参与的邀约活动有所变动，请查看详情";
			}
		}
		return null;
	}
	
}

