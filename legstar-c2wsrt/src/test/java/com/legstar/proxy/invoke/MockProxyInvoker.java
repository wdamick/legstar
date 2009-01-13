package com.legstar.proxy.invoke;

import java.util.Map;

/**
 * A Mock ProxyInvoker.
 *
 */
public class MockProxyInvoker extends AbstractProxyInvoker {

    /**
     * Constructor.
     * @param config configuration set
     */
    public MockProxyInvoker(
            final Map < String, String > config) {
        super(config);
    }
    
    /** {@inheritDoc}*/
    public < T > T invoke(final String requestID, final Object request)
            throws ProxyInvokerException {
        return null;
    }

}
