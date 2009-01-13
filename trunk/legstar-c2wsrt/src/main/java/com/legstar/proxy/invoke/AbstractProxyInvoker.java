package com.legstar.proxy.invoke;

import java.util.HashMap;
import java.util.Map;

/**
 * Proxy Invokers implement the actual capability to invoke a remote process such
 * as a Web Service operation or a POJO method.
 * <p/>
 * Proxy Invokers are immutable.
 *
 */
public abstract class AbstractProxyInvoker implements IProxyInvoker {

    /**
     * Current set of configuration parameters.
     */
    private Map < String, String > mConfig;
    
    /**
     * Constructor copies configuration parameters into a new set of configuration.
     * This way, the initial set of parameters is what matters for this class 
     * operation.
     * @param config the configuration parameters
     */
    public AbstractProxyInvoker(
            final Map < String, String > config) {
        mConfig = new HashMap < String, String >();
        mConfig.putAll(config);
        
    }

    /** {@inheritDoc} */
    public Map < String, String > getConfig() {
        return mConfig;
    }

    /**
     * Determines if a new configuration set is compatible with the one this
     * proxy invoker was setup with.
     * These are the conditions applied:
     * <ul>
     * <li>If no configuration exists yet this will return false.</li>
     * <li>If the new configuration is null, returns false.</li>
     * <li>All keys from current configuration must yield the same value from
     * both existing configuration and new one for this to return true.</li>
     * <li>Both old and new config must have the same number of keys for
     * this to return true.</li>
     * </ul>
     * @param newConfig a proposed new configuration
     * @return if new configuration is identical to the one already used
     */
    public boolean isSameConfig(final Map < String, String > newConfig) {
        if (getConfig() == null || newConfig == null) {
            return false;
        }
        for (String key : getConfig().keySet()) {
            if (!getConfig().get(key).equals(newConfig.get(key))) {
                return false;
            }
        }
        if (getConfig().keySet().size() != newConfig.keySet().size()) {
            return false;
        }
        return true;
    }

}
