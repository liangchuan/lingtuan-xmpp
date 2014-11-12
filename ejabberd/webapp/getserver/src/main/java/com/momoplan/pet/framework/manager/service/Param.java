package com.momoplan.pet.framework.manager.service;

import java.util.Map;
import java.util.UUID;


public class Param {
	public String deviceToken ;
	public String msg;
	public String pwd;
	public boolean debug;
	public Map<String,String> params;
	public int badge;
	public String cert;
	public Param(String deviceToken, String msg, String pwd, boolean debug,Map<String, String> params, int badge, String cert) {
		this.deviceToken = deviceToken;
		this.msg = msg;
		this.pwd = pwd;
		this.debug = debug;
		this.params = params;
		this.badge = badge;
		this.cert = cert;
	}
	
	public static void main(String[] args) throws InterruptedException {

		while(true){
			PushApnQueue.test.put(UUID.randomUUID().toString());
			Thread.sleep(100);
		}
		
	}
}