package com.momoplan.pet.framework.manager.node;

import java.io.IOException;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

@Component
public class LogNode {
	
	private static Logger logger = LoggerFactory.getLogger(LogNode.class);
	private String nodeCookie = null;
	private String nodePing = null;
	private String nodeName = null;
	private String nodeBox = null;
	
	@Autowired
	public LogNode(String nodeCookie, String nodePing, String nodeName,String nodeBox) {
		super();
		this.nodeCookie = nodeCookie;
		this.nodePing = nodePing;
		this.nodeName = nodeName;
		this.nodeBox = nodeBox;
	}

	@PostConstruct
	private void start_link() throws Exception{
		new Thread(new Runnable(){
			@Override
			public void run() {
				try {
					OtpNode node = new OtpNode(nodeName, nodeCookie);
					node.ping(nodePing, 5000);
					OtpMbox box = node.createMbox(nodeBox);
					logger.debug("start_link...nodeName="+nodeName);
					logger.debug("start_link...nodeCookie="+nodeCookie);
					logger.debug("start_link...nodePing="+nodePing);
					logger.debug("start_link...nodeBox="+nodeBox);
					while(true){
						try{
							//tuple ::> {id,from,to,msgtype,body}
							OtpErlangObject obj = box.receive();
							OtpErlangTuple tuple = (OtpErlangTuple)obj;
							OtpErlangString id = (OtpErlangString)tuple.elementAt(0);
							OtpErlangString from = (OtpErlangString)tuple.elementAt(1);
							OtpErlangString to = (OtpErlangString)tuple.elementAt(2);
							OtpErlangString msgtype = (OtpErlangString)tuple.elementAt(3);
							OtpErlangBinary bin = (OtpErlangBinary)tuple.elementAt(4);
							String body = new String(bin.binaryValue());
							StringBuffer log = new StringBuffer("\02");
							log.append(id.stringValue()).append("\01");
							log.append(from.stringValue()).append("\01");
							log.append(to.stringValue()).append("\01");
							log.append(msgtype.stringValue()).append("\01");
							log.append(body);
							logger.info(log.toString());
							//TODO write in cassandra
						}catch(Exception e){
							logger.error("error", e);
						}
					}				
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		}).start();;
	}
	
}