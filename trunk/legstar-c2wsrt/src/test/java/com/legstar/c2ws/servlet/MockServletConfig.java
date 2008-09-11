package com.legstar.c2ws.servlet;

import java.util.Enumeration;
import java.util.Properties;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;

/**
 * Simple Mock for ServletConfig.
 */
public class MockServletConfig implements ServletConfig {
	
	private final ServletContext  mServletContext = new MockServletContext();
	private final Properties  mInitParameters = new Properties ();

	public String getInitParameter(String name) {
		return mInitParameters.getProperty(name);
	}

	public Enumeration < ? > getInitParameterNames() {
		return mInitParameters.keys();
	}

    public void addInitParameter(String  name, String  value) {
    	mInitParameters.setProperty(name, value);
   }

    public ServletContext getServletContext() {
		return mServletContext;
	}

	public String getServletName() {
		// TODO Auto-generated method stub
		return null;
	}

}
