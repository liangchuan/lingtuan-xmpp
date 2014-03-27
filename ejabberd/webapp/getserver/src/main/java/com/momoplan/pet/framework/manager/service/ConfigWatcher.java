package com.momoplan.pet.framework.manager.service;

import java.io.IOException;
import java.io.InputStream;
import java.lang.management.ManagementFactory;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.zookeeper.KeeperException;
import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.Watcher;
import org.apache.zookeeper.Watcher.Event.EventType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jdi.Bootstrap;

/**
 * 静态 KEY-VALUE 属性集中管理客户端
 * 通过zookeeper来统一设置属性，通过此客户端将来接受zookeeper的通知
 * 并将属性存入当前JVM的内存
 * <br>
 * 所有配置的属性均存放在 /config-manager 路径下
 * 例如：/config-manager/sms.username 下存放了短信服务的帐号
 * 可以通过 ConfigWatcher.getProperty("sms.username",null) 来获取
 * @author liangc
 */
public class ConfigWatcher extends Config implements Watcher {
	
	private static Logger logger = LoggerFactory.getLogger(ConfigWatcher.class);
	
	private ActiveKeyValueStore store = null;

	/**
	 * 配置文件存放的基础路径，如果应用之间没有公共的配置，应该设置应用的上下文名称；
	 * 源地址，统一用这一个即可
	 */
	private String basePath = "/ejabberd-node";
	/**
	 * 必须填写，要连接的 server
	 * ip:port,ip:port,... default port is 2181
	 */
	private String host = null;
	
	/**
	 * 机器名字 uname -a
	 * 私有属性存放在 uname 对应的 value 中
	 */
	private String uname = null;
	/**
	 * 当前JVM进程所监听的端口;
	 * <br>
	 * TODO 一个进程可以监听多个端口,这种做法挺不科学
	 * 私有属性可以由 uname:pidPort 进行覆盖
	 */
	private List<String> pidPort = new ArrayList<String>();

	public ConfigWatcher() throws IOException, InterruptedException, KeeperException {
		
	}
	
	public ConfigWatcher(String host) throws IOException, InterruptedException, KeeperException {
		setHost(host);
	}
	
	/**
	 * 获取属性，
	 * 优先获取私有属性，私有属性不存在时，获取公有属性，都不存在时，返回默认值
	 * @param key
	 * @return
	 */
	public String getProperty(String key,String def){
		String v = getPrivateProperty(key);
		if(StringUtils.isEmpty(v)){
			v = getPublicProperty(key);
		}
		if(StringUtils.isEmpty(v)){
			v = def;
		}
		return v;
	}
	/**
	 * 获取公共的属性，所有机器用的属性都一样
	 * @param key
	 * @return
	 */
	public String getPublicProperty(String key){
		return publicConfig.get(key);
	}
	/**
	 * 获取私有属性，属性的KEY下面会按照机器的HOSTNAME来存储对应的值
	 * @param key
	 * @return
	 */
	public String getPrivateProperty(String key){
		return privateConfig.get(key);
	}
	
	public Set<String> getPublicPropertyKeys(){
		return publicConfig.keySet();
	}

	public Set<String> getPrivatePropertyKeys(){
		return publicConfig.keySet();
	}

	/**
	 * 事件处理 
	 */
	@Override
	public void process(WatchedEvent event) {
		String path = "";
		String key = "";
		String value = "";
		String env = "";
		if(event.getType()==EventType.NodeDataChanged){//node值变化事件
			try {
				env = "NodeDataChanged";
				path = event.getPath();
				value = store.read( path, this);
				if(path.contains(uname)){
					String[] p = path.split("/");
					key = p[p.length-2];
					if(path.equalsIgnoreCase(privateConfigReg.get(key))){
						privateConfig.put(key, value);
						logger.debug(basePath+" : "+"Overwrites : "+path);
					}
					logger.debug(basePath+" : "+"Chang Path : "+path);
					logger.debug(basePath+" : "+"Reg Path : "+privateConfigReg.get(key));
				}else{
					String[] p = path.split("/");
					key = p[p.length-1];
					publicConfig.put(key, value);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else if(event.getType()==EventType.NodeDeleted){//node删除事件
			try {
				env = "NodeDeleted";
				path = event.getPath();
				if(path.contains(uname)){
					String[] p = path.split("/");
					key = p[p.length-2];
					privateConfig.remove(key);
					privateConfigReg.remove(key);
				}else{
					String[] p = path.split("/");
					key = p[p.length-1];
					publicConfig.remove(key);
				}
				eachBasePath();
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else if(event.getType()==EventType.NodeChildrenChanged){//node子节点变化事件
			env = "NodeChildrenChanged";
			try {
				eachBasePath();
			} catch (KeeperException e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		} else if(event.getType()==EventType.NodeCreated){//node创建事件
			try {
				env = "NodeCreated";
				path = event.getPath();
				value = store.read( path, this);
				if(path.contains(uname)){
					String[] p = path.split("/");
					key = p[p.length-2];
					privateConfig.put(key, value);
					privateConfigReg.put(key, path);
				}else{
					String[] p = path.split("/");
					key = p[p.length-1];
					publicConfig.put(key, value);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		logger.debug(basePath+" : "+"publicConfig : "+publicConfig);
		logger.debug(basePath+" : "+"privateConfig : "+privateConfig);
		logger.info(basePath+" : "+"Config "+env+" : path="+path+" ; "+key+"="+value+"\n");
	}
	
	private void init() throws IOException, InterruptedException, KeeperException {
		store = new ActiveKeyValueStore();
		store.connect(getHost());
		uname = (InetAddress.getLocalHost()).getHostName();
//		if(Bootstrap.http_port>0){
//			pidPort.add(Bootstrap.http_port+"");
//		}else{
//			String pid = ManagementFactory.getRuntimeMXBean().getName();
//			fillPidPort(pid);
//		}
		pidPort.add("8080");
		logger.debug(basePath+" : "+"basePath : "+basePath);
		eachBasePath();
	}
	/**
	 * 遍历配置节点的所有子节点
	 * @throws KeeperException
	 * @throws InterruptedException
	 */
	private void eachBasePath() throws KeeperException, InterruptedException{
		store.write(basePath, null);
		List<String> list = store.zk.getChildren(basePath, this);
		for(String path : list){
			//通过机器名将相同的KEY以机器名分开
			String private_value = store.read(basePath+"/"+path+"/"+uname, this);
			String privateReg = basePath+"/"+path+"/"+uname;//私有属性的注册名，如果值发生变化时，可以通过这个注册名找到私有属性的 path
			String public_value = store.read(basePath+"/"+path, this);
			//通过端口将相同的机器名和相同的KEY以当前JVM进程所绑定的端口号进行区分
			if(pidPort!=null&&pidPort.size()>0){
				for(String port:pidPort){
					//监听这个进程对应的每一个端口，并取值
					String pv = store.read(basePath+"/"+path+"/"+uname+":"+port, this);
					//如果端口对应的路径有值，则覆盖原有的值，端口可以是多个，相互无序覆盖
					//所以用端口来区分服务的时候，要注意，同一个进程的多个端口相互覆盖的问题
					if(StringUtils.isNotEmpty(pv)){
						private_value = pv;
						privateReg = basePath+"/"+path+"/"+uname+":"+port;
					}
				}
			}
			publicConfig.put(path, public_value);
			privateConfig.put(path, private_value);
			privateConfigReg.put(path,privateReg);
		}
		logger.debug(basePath+" : "+"eachBasePath() publicConfig "+publicConfig);
		logger.debug(basePath+" : "+"eachBasePath() privateConfig "+privateConfig);
		logger.debug(basePath+" : "+"eachBasePath() privateConfigReg "+privateConfigReg);
	}
	
	public String getHost() {
		return host;
	}

	public void setHost(String host) throws IOException, InterruptedException, KeeperException {
		this.host = host;
		init();
	}

	/**
	 * 获取属性
	 * <br>
	 * 2013-10-08:这个方法使用时有局限性，如果配合spring的web项目一起使用，则可废弃此方法，用spring的启动监听来设置端口
	 * @param key 属性名
	 * @param def 默认值
	 * @return
	 */
	@Deprecated
	private void fillPidPort(String pid) throws IOException{
		String cmd = "netstat -anp |grep #pid# |grep LISTEN|awk '{print $4}'|awk -F ':' '{print $NF}'";
		cmd = cmd.replaceFirst("#pid#", pid);
		logger.debug(basePath+" : "+"fillPidPort [cmd] -->  "+cmd);
		Process process = Runtime.getRuntime().exec(new String[]{"sh","-c",cmd});
		InputStream is = process.getInputStream();
		pidPort = IOUtils.readLines(is);
		Collections.sort(pidPort);
		is.close();
	}

	public String getBasePath() {
		return basePath;
	}

	public void setBasePath(String basePath) {
		this.basePath = basePath;
	}
	
	public void close() throws InterruptedException{
		store.close();
	}
	public ActiveKeyValueStore getStore() {
		return store;
	}

	public void setStore(ActiveKeyValueStore store) {
		this.store = store;
	}
	
	public static void main(String[] args) throws Exception {
		/**
		 * sms.properties
		 * -----------------------
		 * sms.username=J00348
		 * sms.password=142753
		 */
		ConfigWatcher app1 = new ConfigWatcher("123.178.27.74:2181");

		app1.store.write(app1.getBasePath()+"/ejabberd@s1", "192.168.0.60:5222");
		
		logger.info("app1 publicConfig : "+Config.publicConfig);
		logger.info("app1 privateConfig : "+Config.privateConfig);
		logger.info("app1 privateConfigReg : "+Config.privateConfigReg);
		Thread.sleep(Long.MAX_VALUE);
	}
}