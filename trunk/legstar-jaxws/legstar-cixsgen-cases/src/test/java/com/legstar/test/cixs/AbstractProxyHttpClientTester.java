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
package com.legstar.test.cixs;

/**
 * Tests a proxy using a raw HTTP Client.
 *
 */
public abstract class AbstractProxyHttpClientTester extends AbstractHttpClientTester {
    
    /** Name of target adapter web service. */
    private String _wsName;
    
    /** The request payload. */
    private byte[] _bytesRequest;
    
    /**
     * @param wsName name of target adapter web service
     * @param bytesRequest the request payload
     */
    public AbstractProxyHttpClientTester(
            final String wsName, final byte[] bytesRequest) {
        super();
        _wsName = wsName;
        _bytesRequest = bytesRequest;
    }
    /**
     * Perform a complete request.
     */
    public void testRoundTrip() {
        try {
            byte[] replyBytes = postBytes(getServiceURI(), _bytesRequest);
            check(replyBytes);
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.toString());
        }
    }
    
    /**
     * Check that the reply is ok.
     * @param replyBytes the reply payload
     */
    public abstract void check(final byte[] replyBytes);
    
    /**
     * @return the proxy service location.
     */
    protected String getServiceURI() {
        return "http://" + getJ2EEHost() + ":8080/c2ws-" + _wsName + "/" + _wsName + "Proxy";
    }

    /**
     * @return the name of target adapter web service
     */
    public String getWsName() {
        return _wsName;
    }
    
}
