package com.momoplan.pet.framework.manager.service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javapns.Push;
import javapns.devices.Device;
import javapns.devices.implementations.basic.BasicDevice;
import javapns.notification.Payload;
import javapns.notification.PushNotificationPayload;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
/**
 * 给 IOS 推送消息
 * @author liangc
 */
public class PushApn {
	
	private static Logger logger = LoggerFactory.getLogger(PushApn.class);
	
	static String def_cert = System.getProperty("user.home")+"/.ssh/ynm.p12";
	/**
	 * 单发消息
	 * @param deviceToken 
	 * @param msg 
	 * @param pwd 密码 123456
	 */
	public static void sendMsgApn(String deviceToken,String msg,String pwd,boolean debug,Map<String,String> params,int badge,String cert) throws Exception {
		try {
			if(cert==null||"".equals(cert)){
				cert = def_cert;
				logger.debug("use_def_cert");
			}else{
				if(cert.startsWith("dis")){
					debug = false;
				}else{
					debug = true;
				}
				cert = System.getProperty("user.home")+"/.ssh/"+cert;
			}
			PushNotificationPayload payLoad = new PushNotificationPayload();
			payLoad.addSound("default"); // 铃音 默认
			payLoad.addBadge(badge);
			payLoad.addAlert(msg);
			if(params!=null){
				for(String k : params.keySet()){
					String v = params.get(k);
					payLoad.addCustomDictionary(k, v);
				}
			}
			Device device = new BasicDevice();
			device.setToken(deviceToken);
			//2014-7-10 : 确定不了哪个是生产哪个是测试，而且总改证书名和密码，所以 生产/测试 都试一下，必然影响效率
			Push.payload(payLoad, cert, pwd , true , device);
			Push.payload(payLoad, cert, pwd , false , device);
			logger.info("deviceToken="+deviceToken+" ; cert="+cert+" ; pwd="+pwd+" ; debug="+debug+" ; msg="+msg+" ; params="+params); 
		} catch (Exception e) {
			logger.error("push_error",e);
			throw e;
		}
	}

	public static void sendMsgApn(String msg,String pwd,boolean debug,Map<String,String> params, int threads, List<Device> devices ,int badge ) throws Exception{
		String cert = System.getProperty("user.home")+"/.ssh/ynm.p12";
		PushNotificationPayload payLoad = new PushNotificationPayload();
		payLoad.addSound("default"); // 铃音 默认
		payLoad.addBadge(badge);
		payLoad.addAlert(msg);
		if(params!=null){
			for(String k : params.keySet()){
				String v = params.get(k);
				payLoad.addCustomDictionary(k, v);
			}
		}
		Push.payload(payLoad, cert , pwd, !debug, threads,devices);
	}
	
	//(String deviceToken,String msg,String pwd,boolean debug,Map<String,String> params) throws Exception {

	public static void main(String[] args) throws Exception {
		long s = System.currentTimeMillis();
		//String token = "588a5c9d98addf7aea3b0dc391bdc0a9c004a9d5bcdf83b095614463859d1079";
		String token = "6c36310d6b2b9bee17506268240586d3695dd9a43af044505bee09db549e3268";
		//{to=480924@yuejian.net/46E283A9-6472-4A6B-A6FB-BFED1AC8F35C, id=EC87258C-940C-459F-90AD-6692D3714F87, msgtype=normalchat, from=404757@yuejian.net/46E283A9-6472-4A6B-A6FB-BFED1AC8F35C}
		Map<String,String> map = new HashMap<String,String>();
//		map.put("to", "480924@yuejian.net/46E283A9-6472-4A6B-A6FB-BFED1AC8F35C");
//		map.put("from", "404757@yuejian.net");
//		map.put("id", "EC87258C-940C-459F-90AD-6692D3714F87");
		map.put("msgtype", "normalchat");
//		sendMsgApn(token,"liangc-test-001","111111",true,map,1);
		sendMsgApn(token,"liangc-test-002","111111",false,map,1,null);
		
		System.out.println("OK...");
		long e = System.currentTimeMillis();
		System.out.println(e-s);
	}
	
}
