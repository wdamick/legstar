/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
 * Proxy invokers implement the mechanisms to actually invoke a process on
 * behalf of a client.
 *
 */
public interface IProxyInvoker {

    /** Property giving the proxy invoker class name from configuration properties. */ 
    String PROXY_INVOKER_CLASS_NAME_PROPERTY = "proxyInvokerClassName";

    /**
     * Invoke a process.
     * @param <T> the reply type
     * @param requestID a unique identifier for this request
     * @param oRequest the java data object representing the request data
     * @return a java data object representing the reply
     * @throws ProxyInvokerException if invoke fails
     */
    < T > T invoke(final String requestID, final Object oRequest) throws ProxyInvokerException;
    
    /**
     * @return the current configuration parameter set for this invoker
     */
    Map < String, String > getConfig();
    
    /**
     * Determines if a new configuration is identical to the currently used one.
     * This allows callers to reuse proxy providers when possible.
     * @param newConfig a proposed new configuration
     * @return if new configuration is identical to the one already used
     */
    boolean isSameConfig(Map < String, String > newConfig);
    
}
