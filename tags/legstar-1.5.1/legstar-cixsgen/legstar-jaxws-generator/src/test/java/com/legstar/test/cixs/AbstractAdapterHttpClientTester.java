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

import java.io.File;

import org.codehaus.cargo.container.InstalledLocalContainer;
import org.codehaus.cargo.container.configuration.LocalConfiguration;
import org.codehaus.cargo.container.configuration.entry.Resource;

/**
 * Tests a Web Service adapter using a raw HTTP Client. This reduces the
 * overhead as opposed to using a JAX-WS client.
 * 
 */
public abstract class AbstractAdapterHttpClientTester extends
        AbstractHttpClientTester {

    /** Name of target adapter web service. */
    private String _wsName;

    /** The SOAP request expected by target Web Service (captured by TCPMon). */
    private String _soapRequest;

    /** The SOAP reply produced by target Web Service (captured by TCPMon). */
    private String _soapReply;

    /** The type of transport used to reach the mainframe. */
    private String _endpointName;

    /**
     * @param wsName name of target adapter web service
     * @param soapRequest request expected by target Web Service
     * @param soapReply reply produced by target Web Service
     */
    public AbstractAdapterHttpClientTester(final String wsName,
            final String soapRequest, final String soapReply) {
        super();
        _wsName = wsName;
        _soapRequest = soapRequest;
        _soapReply = soapReply;
    }

    /**
     * CICSTS31 Direct Http.
     */
    public void testRoundTripCICSTS31DirectHttp() {
        _endpointName = "CICSTS31DirectHttp";
        performRoundTrip();
    }

    /**
     * CICSTS31 Pooled Http.
     */
    public void testRoundTripCICSTS31PooledHttp() {
        _endpointName = "CICSTS31PooledHttp";
        performRoundTrip();
    }

    /**
     * CICSTS23 Direct Http.
     */
    public void testRoundTripCICSTS23DirectHttp() {
        _endpointName = "CICSTS23DirectHttp";
        performRoundTrip();
    }

    /**
     * CICSTS23 Pooled Http.
     */
    public void testRoundTripCICSTS23PooledHttp() {
        _endpointName = "CICSTS31DirectHttp";
        performRoundTrip();
    }

    /**
     * CICSTS23 Direct Socket.
     */
    public void testRoundTripCICSTS23DirectSocket() {
        _endpointName = "CICSTS23DirectSocket";
        performRoundTrip();
    }

    /**
     * CICSTS23 Pooled Socket.
     */
    public void testRoundTripCICSTS23PooledSocket() {
        _endpointName = "CICSTS23PooledSocket";
        performRoundTrip();
    }

    /**
     * CICSTS23 Direct MQ.
     */
    public void testRoundTripCICSTS23DirectMQ() {
        _endpointName = "CICSTS23DirectMQ";
        performRoundTrip();
    }

    /**
     * CICSTS23 Pooled MQ.
     */
    public void testRoundTripCICSTS23PooledMQ() {
        _endpointName = "CICSTS23PooledMQ";
        performRoundTrip();
    }

    /**
     * CICSTS31 Direct MQ.
     */
    public void testRoundTripCICSTS31DirectMQ() {
        _endpointName = "CICSTS31DirectMQ";
        performRoundTrip();
    }

    /**
     * CICSTS31 Pooled MQ.
     */
    public void testRoundTripCICSTS31PooledMQ() {
        _endpointName = "CICSTS31PooledMQ";
        performRoundTrip();
    }

    /**
     * Mock Direct.
     */
    public void testRoundTripMockDirect() {
        _endpointName = "MockDirect";
        performRoundTrip();
    }

    /**
     * Mock Pooled.
     */
    public void testRoundTripMockPooled() {
        _endpointName = "MockPooled";
        performRoundTrip();
    }

    /**
     * Perform a complete request.
     * 
     * @throws Exception if request fails
     */
    public void callServiceAndCheck() throws Exception {
        String xmlReply = postXml(getServiceURI(),
                _soapRequest.replace("${endpointName}", _endpointName), "urn:"
                        + _wsName);
        assertEquals(_soapReply, xmlReply);
    }

    /**
     * @return the web service location.
     */
    protected String getServiceURI() {
        return "http://" + getJ2EEHost() + ":8080/cixs-" + _wsName + "/"
                + _wsName;
    }

    /**
     * @return the name of target adapter web service
     */
    public String getWsName() {
        return _wsName;
    }

    /**
     * Add WebSphere MQ resources to Tomcat.
     * 
     * @return a container ready to start
     */
    @SuppressWarnings("unchecked")
    public InstalledLocalContainer getContainer() {
        InstalledLocalContainer webapp = super.getContainer();

        File wmqDir = new File(System.getenv("WMQ_HOME") + "/lib");
        webapp.addSharedClasspath(new File(wmqDir, "com.ibm.mq.jar")
                .getAbsolutePath());
        webapp.addSharedClasspath(new File(wmqDir, "com.ibm.mqjms.jar")
                .getAbsolutePath());
        webapp.addSharedClasspath(new File(wmqDir, "dhbcore.jar")
                .getAbsolutePath());

        LocalConfiguration configuration = webapp.getConfiguration();
        Resource dsConnectionFactory = new Resource("ConnectionFactory",
                "com.ibm.mq.jms.MQQueueConnectionFactory");
        dsConnectionFactory.getParameters().put("hostName", "mainframe");
        dsConnectionFactory.getParameters().put("port", "1414");
        dsConnectionFactory.getParameters().put("queueManager", "CSQ1");
        dsConnectionFactory.getParameters().put("channel", "CLIENT.TO.CSQ1");
        dsConnectionFactory.getParameters().put("transportType", "1");
        configuration.addResource(dsConnectionFactory);

        configuration.addResource(getQueueResource("CicsARequestQueue",
                "CICSA.REQUEST.QUEUE"));
        configuration.addResource(getQueueResource("CicsAReplyQueue",
                "CICSA.REPLY.QUEUE"));
        configuration.addResource(getQueueResource("Cics01BridgeRequestQueue",
                "CICS01.BRIDGE.REQUEST.QUEUE"));
        configuration.addResource(getQueueResource("Cics01BridgeReplyQueue",
                "CICS01.BRIDGE.REPLY.QUEUE"));
        return webapp;
    }

    /**
     * Create a Tomcat resource for a WMQ queue.
     * 
     * @param jndiName the queue JNDI name
     * @param queueName the queue name (in WMQ)
     * @return a Tomact resource
     */
    @SuppressWarnings("unchecked")
    protected Resource getQueueResource(final String jndiName,
            final String queueName) {
        Resource dsQueue = new Resource(jndiName, "com.ibm.mq.jms.MQQueue");
        dsQueue.getParameters().put("baseQueueName", queueName);
        dsQueue.getParameters().put("targetClient", "1");
        return dsQueue;
    }

}
