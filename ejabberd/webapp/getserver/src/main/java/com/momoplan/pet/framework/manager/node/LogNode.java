package com.momoplan.pet.framework.manager.node;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

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
							String res = PostRequest.postText(url, args);
							apns_logger.debug(id+"::>res="+res);
							JSONObject result = new JSONObject(res);
							if(result.getBoolean("success")){
								String deviceToken = result.getString("devicetoken");
								if(StringUtils.isNotEmpty(deviceToken)){
									Map<String,String> map = new HashMap<String,String>();
									map.put("id", id);
									map.put("from", from);
									map.put("to", to);
									map.put("msgtype", msgtype);
									apns_logger.debug(id+"::>push="+msg+" ; map="+map);
									PushApn.sendMsgApn(deviceToken, msg, "123456", isDebug(id), map);
								}
							}
						}catch(Exception e){
							apns_logger.error("push_error",e);
						}
					}
				} catch (Exception e) {
					
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
}