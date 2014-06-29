package com.momoplan.pet.framework.manager.node;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.apache.commons.lang.StringUtils;
import org.apache.zookeeper.KeeperException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.momoplan.pet.framework.manager.node.apnspush.ApnsPushLog;
import com.momoplan.pet.framework.manager.service.Config;
import com.momoplan.pet.framework.manager.service.PostRequest;
import com.momoplan.pet.framework.manager.service.PushApn;

@Component
public class LogNode {
	
	//用这个buff缓存1000秒的消息ID，过滤1000秒内的重复消息
	private Map<Long,Map<String,Long>> record_buff = new HashMap<Long,Map<String,Long>>();
	private boolean is_repeat(String id){
		//1000 秒区间端
		long l = new Date().getTime()/1000000;
		Map<String,Long> buff = record_buff.get(l);
		//clear buff
		if(record_buff.size()>1){
			Set<Long> keys = record_buff.keySet();
			for(Long k : keys){
				if(k!=l){
					apns_logger.debug("remove_timeout_buff k="+k+" ; l="+l); 
					record_buff.remove(k);
				}
			}
		}
		if(buff==null){
			buff = new HashMap<String,Long>();
			buff.put(id, new Date().getTime());
			record_buff.put(l, buff);
			apns_logger.debug("is_repeat=false ; id="+id+" init_buff");
			return false;
		}else if(buff.get(id)==null){
			apns_logger.debug("is_repeat=false ; id="+id+" ; buff.get(id)="+buff.get(id));
			return false;
		}
		apns_logger.debug("is_repeat=true ; id="+id+" ; buff.get(id)="+buff.get(id));
		return true;
	}
	private static Logger msg_logger = LoggerFactory.getLogger(LogNode.class);
	private static Logger apns_logger = LoggerFactory.getLogger(ApnsPushLog.class);

	private static Logger logger = LoggerFactory.getLogger(LogNode.class);
	private String nodeCookie = null;
	private String nodePing = null;
	private String nodeName = null;
	private String nodeBox = null;

	@Autowired
	public LogNode(String nodeCookie, String nodePing, String nodeName,
			String nodeBox) throws IOException, InterruptedException, KeeperException {
		super();
		this.nodeCookie = nodeCookie;
		this.nodePing = nodePing;
		this.nodeName = nodeName;
		this.nodeBox = nodeBox;
	}

	@PostConstruct
	private void start_link() throws Exception {
		new Thread(new Runnable() {

			private void msg_log(OtpErlangObject obj) {
				try {
					// tuple ::> {id,from,to,msgtype,body}
					OtpErlangTuple tuple = (OtpErlangTuple) obj;
					OtpErlangString id = (OtpErlangString) tuple.elementAt(0);
					OtpErlangString from = (OtpErlangString) tuple.elementAt(1);
					OtpErlangString to = (OtpErlangString) tuple.elementAt(2);
					OtpErlangString msgtype = (OtpErlangString) tuple.elementAt(3);
					OtpErlangBinary bin = (OtpErlangBinary) tuple.elementAt(4);
					String body = new String(bin.binaryValue());
					StringBuffer log = new StringBuffer("\02");
					log.append(id.stringValue()).append("\01");
					log.append(from.stringValue()).append("\01");
					log.append(to.stringValue()).append("\01");
					log.append(msgtype.stringValue()).append("\01");
					log.append(body);
					msg_logger.info(log.toString());
					// TODO write in cassandra
				} catch (Exception e) {
					logger.debug("msg_error", e);
				}
			}

			private void apns_push(OtpErlangObject obj) {
				try {
					// tuple ::> {apns_push:atom,id:string,from:string,to:string,msgtype:string,msg:string}
					OtpErlangTuple tuple = (OtpErlangTuple) obj;
					OtpErlangAtom flag = (OtpErlangAtom) tuple.elementAt(0);
					if ("apns_push".equalsIgnoreCase(flag.atomValue())) {
						String id = ((OtpErlangString) tuple.elementAt(1)).stringValue();
						String from = ((OtpErlangString) tuple.elementAt(2)).stringValue();
						String to = ((OtpErlangString) tuple.elementAt(3)).stringValue();
						String msgtype = ((OtpErlangString) tuple.elementAt(4)).stringValue();
						OtpErlangBinary bin = ((OtpErlangBinary) tuple.elementAt(5));
						String msg = new String(bin.binaryValue());

						int badge = 1;
						try{
							OtpErlangInt badgeObj = ((OtpErlangInt) tuple.elementAt(6));
							badge = badgeObj.intValue();
							apns_logger.debug("has_badge id="+id+" ; badge="+badge);
						}catch(Exception e){}

						if(is_repeat(id)){
							//重复的
							apns_logger.debug("repeat id="+id);
						}else{
							StringBuffer log = new StringBuffer("\02");
							log.append(id).append("\01");
							log.append(from).append("\01");
							log.append(to).append("\01");
							log.append(msgtype).append("\01");
							log.append(msg).append("\01");
							apns_logger.info(log.toString());
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
								apns_logger.debug(id+"::>res="+res);
								JSONObject result = new JSONObject(res);
								if(result.getBoolean("success")){
									String deviceToken = result.getString("devicetoken");
									if(StringUtils.isNotEmpty(deviceToken)){
										Map<String,String> map = new HashMap<String,String>();
//										map.put("id", id);
//										String f = from.split("/")[0];
//										map.put("from", f);
//										map.put("to", to);
										map.put("msgtype", msgtype);
										apns_logger.debug(id+"::>push="+msg+" ; map="+map);
										try{
											String resmsg = build_push_msg(msgtype,msg);
											if(resmsg!=null){
												String[] resmsgArr = resmsg.split("\01");
												if(resmsgArr.length>1){
													apns_logger.debug("if(resmsgArr.length>1)=true");
													deviceToken = resmsgArr[0];
													resmsg = resmsgArr[1];
												}
												//TODO badge
												String apns_push_pwd = Config.publicConfig.get("apns_push_pwd");
												apns_logger.debug(id+"::>push="+msg+" ; map="+map+ " ; resmsg="+resmsg+" ; apns_push_pwd="+apns_push_pwd);
												PushApn.sendMsgApn(deviceToken, resmsg, apns_push_pwd, isDebug(id), map,badge);
											}else{
												apns_logger.debug(id+"::>push="+msg+" ; map="+map+ " ; resmsg="+resmsg);
											}
										}catch(Exception err){
											apns_logger.error(id,err);
											err.printStackTrace();
										}
									}
								}
							}catch(Exception e){
								apns_logger.error("push_error",e);
							}
						}
					}
				} catch (Exception e) {
					logger.debug("apns_error", e);
				}
			}

			@Override
			public void run() {
				try {
					OtpNode node = new OtpNode(nodeName, nodeCookie);
					node.ping(nodePing, 5000);
					OtpMbox box = node.createMbox(nodeBox);
					logger.debug("start_link...nodeName=" + nodeName);
					logger.debug("start_link...nodeCookie=" + nodeCookie);
					logger.debug("start_link...nodePing=" + nodePing);
					logger.debug("start_link...nodeBox=" + nodeBox);
					while (true) {
						try {
							OtpErlangObject obj = box.receive();
							logger.debug("receive = " + obj.toString());
							msg_log(obj);
							apns_push(obj);
						} catch (Exception e) {
							logger.error("error", e);
						}
					}
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		}).start();
		;
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
	
	public static void main(String[] args) throws Exception {
		System.out.println("123"+null);
	}
	private String build_push_msg(String msgtype ,String jmsg) throws Exception{
		JSONObject json = new JSONObject(jmsg);
		String type = json.getString("type");
		if("normalchat".equalsIgnoreCase(msgtype)){
//			1.单人聊天信息:         
			if("0".equals(type)){
//			 <1>文本消息： ------------------logo+约你妹  用户昵称：最新聊天的语言（最多显示61个汉字）
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
//				  <2>图片消息：缩略图=cover------------------logo+约你妹  用户昵称发来一张图片
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"1","content":"url","cover":"base64编码图片"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来一张图片";
			}else if("2".equals(type)){
//				  <3>语音消息：amr格式------------------logo+约你妹  用户昵称发来一段语音
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"2","content":"url","second":"60"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来一段语音";
//			}else if("3".equals(type)){
//				  <4>gif动画消息：------------------logo+约你妹  用户昵称：gift识别出来的文字（暂无）
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"3","content":"url"}
//			     </body>
//			     </message>
				
			}else if("4".equals(type)){
//				  <5>位置消息：------------------  logo+约你妹  用户昵称：【我的位置】
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"4","lon":"39.000","lat":"139.000"}</body> 高德坐标
//			     </message>
				String username = json.getString("username");
				return username+"【我的位置】";
			}else if("5".equals(type)){
//				  <6>场景消息：------------------  logo+约你妹  用户昵称发来了约会场景
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"5","name":"商家名称","address":"商家地址","image":"url","id":"场景id，产品详情页的id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来了约会场景";
			}else if("6".equals(type)){
//				<7>名片消息：------------------  logo+约你妹  用户昵称发来了XXX的名片  
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"6","name":"名片人的用户名","image":"url","id":"用户id",“sign”:"签名"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				String name = json.getString("name");
				return username+"发来了"+name+"的名片";
			}
		}else if("groupchat".equalsIgnoreCase(msgtype)){
//			2.多人会话聊天信息：groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）
			if("0".equals(type)){
//				  <1>文本消息：------------------  logo+约你妹   用户昵称：最新聊天的语言（最多显示61个汉字）
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
//				  <2>图片消息：------------------logo+约你妹  用户昵称发来一张图片
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"1","content":"url","cover":"base64编码图片"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来一张图片";
			}else if("2".equals(type)){
//				  <3>语音消息：------------------logo+约你妹  用户昵称发来一段语音
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"2","content":"url","second":"60"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来一段语音";
//			}else if("3".equals(type)){
//				  <4>gif动画消息：------------------logo+约你妹  用户昵称：gift识别出来的文字
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"3","content":"url"}
//			     </body>
//			     </message>
			}else if("4".equals(type)){
//				  <5>位置消息：------------------  logo+约你妹  用户昵称：【我的位置】
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"4","lon":"39.000","lat":"139.000"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"【我的位置】";
			}else if("5".equals(type)){
//				  <6>场景消息：------------------  logo+约你妹  用户昵称发来了约会场景
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"5","name":"商家名称","address":"商家地址","image":"url","id":"场景id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"发来了约会场景";
			}else if("6".equals(type)){
//				<7>名片消息：------------------  logo+约你妹  用户昵称发来了XXX的名片
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“groupchat”>
//			     <body>{"groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"6","name":"名片人的用户名","image":"url","id":"用户id",“sign”:"签名"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				String name = json.getString("name");
				return username+"发来了"+name+"的名片";
			}
		}else if("system".equalsIgnoreCase(msgtype)){
//			（3）.约你妹系统消息定义?
			if("-1".equals(type)){
				//			1.系统消息:------------------  logo+约你妹  该账号已经在xxx机子上登陆，您被强迫下线
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
//				这个是新定义消息的格式，增加devicetoken字段，你做apns推送这个消息的时候，
//				不要去通过web端提供的获取devkcetoken的接口取做apns推送，而直接用传递过来的devicetoken值，
//				因为你如果去调用web接口获取devicetoken，那这个devicetoken值是新登陆用户的手机的devicetoken值，
//				而不是发送给需要下线用户的手机的devicetoken值。其他消息还是通过web端提供的接口获取devicetoken值。
				String devicetoken = json.getString("devicetoken");
				String content = json.getString("content");
				return devicetoken+"\01"+content;
			}else if("0".equals(type)){
//			     <1>好友请求消息（ejabber不处理)------------------  logo+约你妹  xxx请求你加为好友
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"请求您加为好友";
			}else if("1".equals(type)){
//			     <2>好友请求确认消息（ejabber不处理)------------------  logo+约你妹  xxx已经通过了您的好友请求
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"1"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"已经通过了您的好友请求";
			}else if("2".equals(type)){
//			     <3>好友删除消息（ejabber不处理)------------------  不提示
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"2"}
//			     </body>
//			     </message>
			}else if("3".equals(type)){
//			     <4>邀约请求消息（ejabber需要处理)------------------  logo+约你妹  xxx发来了邀约请求
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
//			     <5>同意邀约请求消息（ejabber不做处理）------------------  logo+约你妹  xxx同意了您的邀约请求
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"4","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"同意了您的邀约请求";
			}else if("5".equals(type)){
//			     <6>拒绝邀约请求消息（ejabber不做处理）------------------  logo+约你妹  xxx拒绝了您的邀约请求，赶快求个安慰
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"5","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"拒绝了您的邀约请求，赶快求个安慰";
			}else if("6".equals(type)){
//			     <7>报名活动消息（ejabber不做处理）------------------  logo+约你妹  xxx报名了您发起的活动
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"6","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"报名了您发起的活动";
			}else if("7".equals(type)){
//			      <8>活动管理者 同意报名用户参加消息（ejabber需要处理）------------------  logo+约你妹  xxx通过了您报名的活动
//			     <message id="xxxxx" from="xx@test.com" to="0@group.test.com type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"7","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id","applylist":[”123456","123456","123456"]}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"通过了您报名的活动";
			}else if("8".equals(type)){
//			     <9>活动管理者 拒绝报名用户参加请求消息（ejabber需要处理）------------------  logo+约你妹  xxx拒绝了您报名的活动
//			     http发送消息格式
//			     <message id="xxxxx" from="xx@test.com" to="0@group.test.com type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"8","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id","applylist":[”123456","123456","123456"]}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"拒绝了您报名的活动";
			}else if("9".equals(type)){
//			     <10>肋友（是好友）牵线邀请消息（ejabber不做处理）------------------  logo+约你妹  有人学雷锋为你牵线发起邀约活动
//                ------------------  logo+约你妹  XXX学雷锋为你牵线发起邀约活动
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
//			    <12>拒绝肋友（是好友）牵线邀请消息（ejabber不做处理）------------------  logo+约你妹  xxx拒绝了您的牵线邀约
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"11","sort":"种类（吃喝玩乐）","guest":"1","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id"}
//			     </body>
//			     </message>
				String username = json.getString("username");
				return username+"拒绝了您的牵线邀约";
			}else if("12".equals(type)){
//			     <13>创建加入多人会话消息（ejabber不需要处理）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）                   ------------------  logo+约你妹  xxx创建了多人对话（消息分发给每一个讨论组人员）
//			     http发送消息格式
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"12"}</body>
//			     </message>
				//凡2014-06-25 15:26:18
				//修改下这两个传递的内容 你被邀请您加入xxxx（xxxx为多人对话名称）
				String groupname = json.getString("groupname");
				return "你被邀请加入"+groupname;
			}else if("13".equals(type)){
//			     <14>邀请某些人加入多人会话消息（ejabber需要处理）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）------------------  logo+约你妹  xxx邀请您加入xxxx（xxxx为群组名称）
//			     http发送消息格式
//			     <message id="xxxxx" from="xx@test.com" to"yy@group.test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"13","grouplist":[”123456","123456","123456"]}</body>
//			     </message>
				//凡2014-06-25 15:26:18
				//修改下这两个传递的内容 你被邀请您加入xxxx（xxxx为多人对话名称）
				String groupname = json.getString("groupname");
				return "你被邀请加入"+groupname;
			}else if("14".equals(type)){
//			     <15>踢出某个多人会话成员消息（ejabber不需要处理，这里特别注意的是自己退出多人对话（非管理员），不发送xmpp消息）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）------------------ logo+约你妹  您被管理员踢出xxxx（xxxx为群组名称）
//			     <message id="xxxxx" from="xx@test.com" to"yy@test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"14"}</body>
//			     </message>
				String groupname = json.getString("groupname");
				return "您被管理员踢出"+groupname;
			}else if("15".equals(type)){
//			     <16>解散自己创建的多人会话消息（ejabber需要处理）groupmember 最多传递5个人的数据（用来显示头像拼接成多人对话图片）
//			     http发送消息格式------------------ logo+约你妹  xxxx已经解散（xxxx为群组名称）
//			     <message id="xxxxx" from="xx@test.com" to="yy@group.test.com" type="normal" msgtype=“system”>
//			     <body>{groupid":"xx","groupname":"群组名称","groupmember":[{"image":"用户头像url","gender":"1"},{"image":"用户头像url","gender":"1"}],"type":"15","grouplist":[”123456","123456","123456"]}</body>
//			     </message>
				String groupname = json.getString("groupname");
				return groupname+"已经解散";
			}else if("16".equals(type)){
//			      <17>活动提醒消息（ejabber不做处理）------------------ logo+约你妹  离活动开始还有xxx分钟，赶快准备赴约吧
//			     <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="normal" msgtype=“system”>
//			     <body>{"userid":"system","type":"16","sort":"种类（吃喝玩乐）","guest":"1",createrid="xx" creatername="张三" createrimage="http://wwww.1.jpg",“createrage”:"23","creatergender":"男",creatersightml":"个性签名","message":"邀请语","address":"商家地址","time":"发起活动时间（时间戳）","id":"活动id","property":"公开","price":"200",“detail”:"商家描述","image":"商家图片url","invitedlist":[{"image":"邀请用户头像url","id":"123456","gender":"1"},{"image":"邀请用户头像url","id":"123456","gender":"1"}]}
//			     </body>
//			     </message>
			}
		}
		return null;
	}
}