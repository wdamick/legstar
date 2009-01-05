package com.legstar.host.invoke;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

/**
 * A super class for service adapters.
 * <p/>
 * Adapters provide one or more methods which map to host programs.
 * This class offers a simple JNDI based setting for the host invokers.
 *
 */
public abstract class AbstractServiceAdapter {
    
    /** An identifier for this service adapter.*/
    private String mServiceAdapterName;
    
    /** The JNDI locator for the invoker configuration file name.*/
    private static final String JNDI_CONFIG_FILE = "java:comp/env/legstar/configFileName";
    
    /** The default configuration file name if not recovered from JNDI. */
    private static final String DEFAULT_CONFIG_FILE = "legstar-invoker-config.xml";

    /** The host invoker configuration file name.*/
    private String mConfigFileName;
    
    /**
     * This constructor attempts to locate a configuration file name from
     * JNDI. If it fails, it falls back to a default configuration file name.
     * @param serviceAdapterName the service adapter identifier
     */
    public AbstractServiceAdapter(final String serviceAdapterName) {
        mServiceAdapterName = serviceAdapterName;
        try {
            Context initCtx = new InitialContext();
            mConfigFileName = (String) initCtx.lookup(JNDI_CONFIG_FILE);
        } catch (NamingException e) {
            mConfigFileName = DEFAULT_CONFIG_FILE;
        }
    }

    /**
     * This constructor gets configuration file name from implementing class.
     * @param serviceAdapterName the service adapter identifier
     * @param configFileName host invoker configuration file name
     */
    public AbstractServiceAdapter(
            final String serviceAdapterName,
            final String configFileName) {
        mServiceAdapterName = serviceAdapterName;
        mConfigFileName = configFileName;
    }

    /**
     * @return a friendly name identifying this service adapter
     */
    public String getServiceAdapterName() {
        return mServiceAdapterName;
    }

    /**
     * @return the host invoker configuration file name
     */
    public String getConfigFileName() {
        return mConfigFileName;
    }

    /**
     * @param configFileName the host invoker configuration file name to set
     */
    public void setConfigFileName(final String configFileName) {
        mConfigFileName = configFileName;
    }

}
