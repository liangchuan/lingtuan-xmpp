package com.momoplan.pet.framework.manager.controller;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.Map;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletResponse;

import org.apache.zookeeper.KeeperException;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import com.momoplan.pet.framework.manager.service.Config;
import com.momoplan.pet.framework.manager.service.ConfigWatcher;

@Controller
public class GetServerController {

	private static Logger logger = LoggerFactory.getLogger(GetServerController.class);
	
	@Autowired
	private String zooServer = null;
	@Autowired
	private String nodeDefHost = null;
	@Autowired
	private String nodeServiceUrl = null;
	
	private JSONObject defNode = new JSONObject();
	
	private ConfigWatcher app = null;
	@PostConstruct
	private void init() throws IOException, InterruptedException, KeeperException, JSONException{
		logger.info("zooServer = "+zooServer);
		app = new ConfigWatcher(zooServer);
		String[] hostArr = nodeDefHost.split(":");
		defNode.put("ip", hostArr[0]);
		defNode.put("port", hostArr[1]);

	}

	@RequestMapping("/best.html")
	public void best(String license, HttpServletResponse response) throws Exception{
		long s = System.currentTimeMillis();
		response.setCharacterEncoding("UTF-8");
		String license_key = "license:"+license;
		logger.debug("license_key="+license_key);
		//TODO check license
		String license_info = Config.publicConfig.get(license_key);
		logger.debug("license_info="+license_info);
		logger.debug("nodeServiceUrl="+nodeServiceUrl);
		URL u = new URL(nodeServiceUrl);
		URLConnection conn = u.openConnection();
		InputStream is = conn.getInputStream();
		byte[] buff = new byte[4096];
		int t = 0;
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		while((t=is.read(buff))!=-1){
			bos.write(buff, 0, t);
		}
		String nodes = new String(bos.toByteArray());
		is.close();
		bos.close();

		JSONObject json = new JSONObject(nodes);
		boolean success = json.getBoolean("success");

		JSONObject rtnJson = new JSONObject();
		rtnJson.put("success",success );
		if(success){
			try{
				JSONArray arr = json.getJSONArray("entity");
				int len = arr.length();
				Map<String,Object> tmp = new HashMap<String,Object>();
				tmp.put("node", "");
				tmp.put("totalCount",Integer.MAX_VALUE);
				for(int i=0;i<len;i++){
					JSONObject node = arr.getJSONObject(i);
					Integer c = node.getInt("totalCount");
					if(c<(Integer)tmp.get("totalCount")){
						tmp.put("node", node.getString("node"));
						tmp.put("totalCount",c);
					}
				}
				logger.debug(tmp.toString());
				String nodeKey = (String)tmp.get("node");
				String ipHost = Config.publicConfig.get(nodeKey);
				String[] hostArr = ipHost.split(":");
				JSONObject entity = new JSONObject();
				entity.put("ip", hostArr[0]);
				entity.put("port", hostArr[1]);
				rtnJson.put("entity",entity);
			}catch(Exception e){
				logger.error("XXXXXX", e);
				rtnJson.put("entity",defNode);
			}
		}else{
			rtnJson.put("entity",json.get("entity"));
		}
		logger.debug(nodes);
		long e = System.currentTimeMillis();
		logger.info("license_key="+license_key+";license_info="+license_info+";rtn="+rtnJson.toString()+" -- "+(e-s)+"ms");
		response.getWriter().write(rtnJson.toString());
	}
	
	
}


