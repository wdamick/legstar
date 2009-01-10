package com.legstar.proxy.invoke;

import java.util.Map;

/**
 * A mock operation proxy.
 *
 */
public class MockOperationProxy extends AbstractOperationProxy {
    
    /**
     * Create the proxy invoker for an operation.
     * @param config the initial set of parameters
     * @throws ProxyConfigurationException if configuration is invalid
     */
    public MockOperationProxy(
            final Map < String, String > config) throws ProxyConfigurationException {
        super(config);
        
    }
    /** {@inheritDoc}*/
    public byte[] invoke(
            final Map < String, String > config,
            final String requestID,
            final byte[] requestBytes) throws ProxyInvokerException {
        return requestBytes;
    }
    
}
