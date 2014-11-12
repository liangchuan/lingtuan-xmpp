package com.momoplan.pet.framework.manager.node;

import java.io.IOException;

import javax.annotation.PostConstruct;

import org.apache.zookeeper.KeeperException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.momoplan.pet.framework.manager.node.apnspush.ApnsPushLog;
import com.momoplan.pet.framework.manager.service.PushApnQueue;

@Component
public class LogNode {
	
	
	
	//用这个buff缓存1000秒的消息ID，过滤1000秒内的重复消息
	//private Map<Long,Map<String,Long>> record_buff = new HashMap<Long,Map<String,Long>>();
//	private boolean is_repeat(String id){
//		//1000 秒区间端
//		long l = new Date().getTime()/1000000;
//		Map<String,Long> buff = record_buff.get(l);
//		//clear buff
//		if(record_buff.size()>1){
//			Set<Long> keys = record_buff.keySet();
//			for(Long k : keys){
//				if(k!=l){
//					apns_logger.debug("remove_timeout_buff k="+k+" ; l="+l); 
//					record_buff.remove(k);
//				}
//			}
//		}
//		if(buff==null){
//			buff = new HashMap<String,Long>();
//			buff.put(id, new Date().getTime());
//			record_buff.put(l, buff);
//			apns_logger.debug("is_repeat=false ; id="+id+" init_buff");
//			return false;
//		}else if(buff.get(id)==null){
//			apns_logger.debug("is_repeat=false ; id="+id+" ; buff.get(id)="+buff.get(id));
//			return false;
//		}
//		apns_logger.debug("is_repeat=true ; id="+id+" ; buff.get(id)="+buff.get(id));
//		return true;
//	}
	
	//private static Logger msg_logger = LoggerFactory.getLogger(LogNode.class);
	private static Logger apns_logger = LoggerFactory.getLogger(ApnsPushLog.class);

	private static Logger logger = LoggerFactory.getLogger(LogNode.class);
	private String nodeCookie = null;
	private String nodePing = null;
	private String nodeName = null;
	private String nodeBox = null;
	
	@Autowired
	private PushApnQueue pushApnQueue = null;
	
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

//			private void msg_log(OtpErlangObject obj) {
//				try {
//					// tuple ::> {id,from,to,msgtype,body}
//					OtpErlangTuple tuple = (OtpErlangTuple) obj;
//					OtpErlangString id = (OtpErlangString) tuple.elementAt(0);
//					OtpErlangString from = (OtpErlangString) tuple.elementAt(1);
//					OtpErlangString to = (OtpErlangString) tuple.elementAt(2);
//					OtpErlangString msgtype = (OtpErlangString) tuple.elementAt(3);
//					OtpErlangBinary bin = (OtpErlangBinary) tuple.elementAt(4);
//					String body = new String(bin.binaryValue());
//					StringBuffer log = new StringBuffer("\02");
//					log.append(id.stringValue()).append("\01");
//					log.append(from.stringValue()).append("\01");
//					log.append(to.stringValue()).append("\01");
//					log.append(msgtype.stringValue()).append("\01");
//					log.append(body);
//					msg_logger.info(log.toString());
//					// TODO write in cassandra
//				} catch (Exception e) {
//					logger.debug("msg_error", e);
//				}
//			}

			

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
							//logger.debug("receive = " + obj.toString());
							OtpErlangTuple tuple = (OtpErlangTuple) obj;
							apns_logger.debug("apns__tuple == "+tuple.toString());
							OtpErlangAtom flag = (OtpErlangAtom) tuple.elementAt(0);
							if ("apns_push".equalsIgnoreCase(flag.atomValue())) {
//								apns_push(obj);
								pushApnQueue.push(obj);
							}
//							try{
//								OtpErlangTuple tuple = (OtpErlangTuple) obj;
//								OtpErlangString id = (OtpErlangString) tuple.elementAt(0);
//								msg_log(obj);
//							}catch(Exception e){
//								apns_push(obj);
//							}
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

}


