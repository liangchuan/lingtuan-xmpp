package com.momoplan.pet.framework.manager.service;

import java.util.HashMap;
import java.util.Map;

public abstract class Config {
	public static Map<String,String> publicConfig = new HashMap<String,String>();
	public static Map<String,String> privateConfig = new HashMap<String,String>();
	public static Map<String,String> privateConfigReg = new HashMap<String,String>();
	public abstract String getProperty(String key,String def);
}