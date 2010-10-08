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
