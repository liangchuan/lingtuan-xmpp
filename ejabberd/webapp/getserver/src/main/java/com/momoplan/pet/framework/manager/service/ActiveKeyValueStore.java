package com.momoplan.pet.framework.manager.service;

import java.nio.charset.Charset;

import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.KeeperException;
import org.apache.zookeeper.Watcher;
import org.apache.zookeeper.ZooDefs.Ids;
import org.apache.zookeeper.data.Stat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ActiveKeyValueStore extends ConnectionWatcher {

	private static Logger logger = LoggerFactory.getLogger(ActiveKeyValueStore.class);

	private static final Charset CHARSET = Charset.forName("UTF-8");
	
	/**
	 * 写入
	 * @param path
	 * @param value
	 * @throws KeeperException
	 * @throws InterruptedException
	 */
	public void write(String path, String value) throws KeeperException, InterruptedException {
		Stat stat = zk.exists(path, false);
		byte[] v = null;
		if(value!=null)
			v = value.getBytes(CHARSET);
		if (stat == null) {
			zk.create(path, v, Ids.OPEN_ACL_UNSAFE, CreateMode.PERSISTENT);
		} else {
			logger.debug(path+" stat is : "+stat);
			zk.setData(path, v , -1);
		}
	}
	
	public String read(String path, Watcher watcher) throws KeeperException, InterruptedException{
		Stat stat = zk.exists(path, watcher);
		if(stat!=null){
			byte[] data = zk.getData(path, watcher, null);
			if(data==null)
				return null;
			return new String(data,CHARSET);
		}
		return null;
	}

	public void delete(String path, Watcher watcher) throws KeeperException, InterruptedException{
		Stat stat = zk.exists(path, watcher);
		if(stat!=null){
			zk.delete(path, stat.getVersion());
		}
	}
	
}
