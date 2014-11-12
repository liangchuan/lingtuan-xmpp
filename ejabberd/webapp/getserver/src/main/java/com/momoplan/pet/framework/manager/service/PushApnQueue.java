package com.momoplan.pet.framework.manager.service;

import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.momoplan.pet.framework.manager.node.apnspush.ApnsPushLog;

public class PushApnQueue {
	
	private static Logger logger = LoggerFactory.getLogger(ApnsPushLog.class);
	
	static BlockingQueue<Param> q = new ArrayBlockingQueue<Param>(300000);
	
	static BlockingQueue<String> test = new ArrayBlockingQueue<String>(300000);
	
	public static void push(String deviceToken, String msg, String pwd, boolean debug,Map<String, String> params, int badge, String cert){
		try {
			PushApnQueue.q.put(new Param(deviceToken, msg, pwd, debug, params, badge, cert));
		} catch (InterruptedException e) {
			logger.error("push_queue_error",e);
			e.printStackTrace();
		}
	}
	
	static {
		for(int i=500;i<1000;i++){
			Thread t = new Thread(new Runnable(){
				@Override
				public void run() {
					while(true){
						try {
							Param t = q.take();
							String name = Thread.currentThread().getName();
							int size = q.size();
							logger.debug("current_thread_name="+name+" ; queue_size="+size);
							PushApn.push(t.deviceToken, t.msg, t.pwd, t.debug, t.params, t.badge, t.cert);
						} catch (InterruptedException e) {
							logger.error("push_thread_error",e);
							e.printStackTrace();
						} catch (Exception e) {
							logger.error("push_thread_error",e);
							e.printStackTrace();
						}
					}
				}
			});
			t.setName("push_apn_thread_"+i);
			t.start();
		}
	}
	
}

