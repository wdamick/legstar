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
 * Tests a Web Service adapter using a raw HTTP Client. This reduces the overhead as
 * opposed to using a JAX-WS client.
 *
 */
public abstract class AbstractAdapterHttpClientTester extends AbstractHttpClientTester {
    
    /** Name of target adapter web service. */
    private String _wsName;
    
    /** The SOAP request expected by target Web Service (captured by TCPMon). */
    private String _soapRequest;

    /** The SOAP reply produced by target Web Service (captured by TCPMon). */
    private String _soapReply;
    
    /**
     * @param wsName name of target adapter web service
     * @param soapRequest request expected by target Web Service
     * @param soapReply reply produced by target Web Service
     */
    public AbstractAdapterHttpClientTester(
            final String wsName, final String soapRequest, final String soapReply) {
        super();
        _wsName = wsName;
        _soapRequest = soapRequest;
        _soapReply = soapReply;
    }

    /**
     * CICSTS31 Direct Http.
     */
    public void testRoundTripCICSTS31DirectHttp() {
        testRoundTrip("CICSTS31DirectHttp");
    }
    
    /**
     * CICSTS31 Pooled Http.
     */
    public void testRoundTripCICSTS31PooledHttp() {
        testRoundTrip("CICSTS31PooledHttp");
    }

    /**
     * CICSTS23 Direct Http.
     */
    public void testRoundTripCICSTS23DirectHttp() {
        testRoundTrip("CICSTS23DirectHttp");
    }
    
    /**
     * CICSTS23 Pooled Http.
     */
    public void testRoundTripCICSTS23PooledHttp() {
        testRoundTrip("CICSTS23PooledHttp");
    }

    /**
     * CICSTS23 Direct Socket.
     */
    public void testRoundTripCICSTS23DirectSocket() {
        testRoundTrip("CICSTS23DirectSocket");
    }
    
    /**
     * CICSTS23 Pooled Socket.
     */
    public void testRoundTripCICSTS23PooledSocket() {
        testRoundTrip("CICSTS23PooledSocket");
    }

    /**
     * CICSTS23 Direct MQ.
     */
    public void testRoundTripCICSTS23DirectMQ() {
        testRoundTrip("CICSTS23DirectMQ");
    }
    
    /**
     * CICSTS23 Pooled MQ.
     */
    public void testRoundTripCICSTS23PooledMQ() {
        testRoundTrip("CICSTS23PooledMQ");
    }

    /**
     * CICSTS31 Direct MQ.
     */
    public void testRoundTripCICSTS31DirectMQ() {
        testRoundTrip("CICSTS31DirectMQ");
    }
    
    /**
     * CICSTS31 Pooled MQ.
     */
    public void testRoundTripCICSTS31PooledMQ() {
        testRoundTrip("CICSTS31PooledMQ");
    }

    /**
     * Mock Direct.
     */
    public void testRoundTripMockDirect() {
        testRoundTrip("MockDirect");
    }
    
    /**
     * Mock Pooled.
     */
    public void testRoundTripMockPooled() {
        testRoundTrip("MockPooled");
    }

    /**
     * Perform a complete request and check result.
     * @param endpointName the target mainframe endpoint name
     */
    private void testRoundTrip(final String endpointName) {
        try {
            String xmlReply = postXml(
                    getServiceURI(),
                    _soapRequest.replace("${endpointName}", endpointName),
                    "urn:" + _wsName);
            assertEquals(_soapReply, xmlReply);
        } catch (Exception e) {
            fail(e.toString());
        }
    }
    
    /**
     * @return the web service location.
     */
    protected String getServiceURI() {
        return "http://" + getJ2EEHost() + ":8080/cixs-" + _wsName + "/" + _wsName;
    }

    /**
     * @return the name of target adapter web service
     */
    public String getWsName() {
        return _wsName;
    }
    
}
