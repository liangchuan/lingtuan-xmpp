package com.momoplan.pet.framework.manager.service;

import java.util.UUID;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.methods.PostMethod;
import org.json.JSONObject;

public class PostRequest {
	
	public static String postText(String url,String ... args) throws Exception{
		HttpClient httpClient = new HttpClient();
		PostMethod post = new PostMethod(url);
		post.getParams().setContentCharset("UTF-8");
		for(int i=0;i<args.length;i+=2){
			String k = args[i];
			String v = args[i+1];
			NameValuePair nvp = new NameValuePair();
			nvp.setName(k);
			nvp.setValue(v);
			post.addParameter(nvp);
		}
		httpClient.executeMethod(post);
		return post.getResponseBodyAsString();
	}
	
	public static void main(String[] argsx) throws Exception {
//		open.yuenimei.com.cn
		String url = "http://open.yuenimei.com.cn/index.php";
		JSONObject json = new JSONObject();
		JSONObject params = new JSONObject();
		params.put("username", "1001");
		json.put("service", "checktoken");
		json.put("method", "getdevicetoken");
		json.put("sn", UUID.randomUUID());
		json.put("params",params);
		String args = json.toString();
		System.out.println(url);
		System.out.println(args);
		String res = PostRequest.postText(url, "body",args);
		System.out.println("res="+res);
		
	}
	
}