package com.momoplan.pet.framework.manager.service;

import java.io.IOException;
import java.util.concurrent.CountDownLatch;

import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.Watcher;
import org.apache.zookeeper.Watcher.Event.KeeperState;
import org.apache.zookeeper.ZooKeeper;

public class ConnectionWatcher implements Watcher {
	
	private static final int SESSION_TIMEOUT = 5000;
	
	public ZooKeeper zk = null;
	private CountDownLatch connectedSignal = new CountDownLatch(1);
	
	public void connect(String connectString) throws IOException, InterruptedException{
		zk = new ZooKeeper(connectString, SESSION_TIMEOUT, this);
		connectedSignal.await();
	}
	
	@Override
	public void process(WatchedEvent event) {
		if( event.getState().equals(KeeperState.SyncConnected)){
			connectedSignal.countDown();
		}
	}
	
	public void close() throws InterruptedException{
		zk.close();
	}
	
}
