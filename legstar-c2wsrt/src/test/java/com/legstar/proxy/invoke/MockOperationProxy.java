/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.proxy.invoke;

import java.util.Map;

/**
 * A mock operation proxy.
 *
 */
public class MockOperationProxy extends AbstractOperationProxy {
    
    /**
     * Serial ID.
     */
    private static final long serialVersionUID = 1L;
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
