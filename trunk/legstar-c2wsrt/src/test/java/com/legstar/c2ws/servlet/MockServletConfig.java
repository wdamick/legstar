package com.legstar.c2ws.servlet;

import java.util.Enumeration;
import java.util.Map;
import java.util.Properties;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;

/**
 * Simple Mock for ServletConfig.
 */
public class MockServletConfig implements ServletConfig {

    /** The inner context. */
    private final ServletContext  mServletContext = new MockServletContext();

    /** Properties set.*/
    private final Properties  mInitParameters = new Properties();

    /** {@inheritDoc} */
    public String getInitParameter(final String name) {
        return mInitParameters.getProperty(name);
    }

    /** {@inheritDoc} */
    public Enumeration < ? > getInitParameterNames() {
        return mInitParameters.keys();
    }

    /** {@inheritDoc} */
    public void addInitParameter(final String  name, final String  value) {
        mInitParameters.setProperty(name, value);
    }
    
    /**
     * Bulk copy of a set of prperty/values.
     * @param config a set of properties
     */
    public void addInitParameters(final Map < String, String > config) {
        mInitParameters.putAll(config);
    }

    /** {@inheritDoc} */
    public ServletContext getServletContext() {
        return mServletContext;
    }

    /** {@inheritDoc} */
    public String getServletName() {
        // TODO Auto-generated method stub
        return null;
    }

}
