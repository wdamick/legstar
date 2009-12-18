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
package com.legstar.mq.client;

import junit.framework.TestCase;

/**
 * Test the WMQ endpoint.
 *
 */
public class CicsMQEndpointTest extends TestCase {

    /**
     * Instantiate from full configuration.
     */
    public void testInstantiation() {
        CicsMQEndpoint cicsMQEndpoint = AbstractMQConnectionTester.getLsmsgEndpoint();
        assertEquals("IBM01140", cicsMQEndpoint.getHostCharset());
        assertEquals("mainframe", cicsMQEndpoint.getHostIPAddress());
        assertEquals(1414, cicsMQEndpoint.getHostIPPort());
        assertEquals("STREAM2", cicsMQEndpoint.getHostPassword());
        assertEquals("CSQ1", cicsMQEndpoint.getHostMQManager());
        assertEquals("CLIENT.TO.CSQ1", cicsMQEndpoint.getHostMQChannel());
        assertEquals("CICSA.REQUEST.QUEUE", cicsMQEndpoint.getHostMQRequestQueue());
        assertEquals("CICSA.REPLY.QUEUE", cicsMQEndpoint.getHostMQResponseQueue());
        assertEquals("P390", cicsMQEndpoint.getHostUserID());
        assertEquals("LSMSG", cicsMQEndpoint.getHostMQBridgeType().toString());
        assertEquals("CICS WMQ endpoint:[hostEndpoint=CICSTS23-LSMSG,"
                + "hostCharset=IBM01140,"
                + "hostUserID=P390,"
                + "hostPassword=********,"
                + "hostTraceMode=false,"
                + "connectTimeout=1000,"
                + "receiveTimeout=5000,"
                + "hostConnectionfactoryClass=com.legstar.mq.client.CicsMQConnectionFactory,"
                + "hostAccessStrategy=direct,"
                + "hostConnectionPoolSize=5,"
                + "pooledInvokeTimeout=3000]"
                + "[hostIPAddress=mainframe,"
                + "hostIPPort=1414,"
                + "hostMQManager=CSQ1,"
                + "hostMQChannel=CLIENT.TO.CSQ1,"
                + "hostMQRequestQueue=CICSA.REQUEST.QUEUE,"
                + "hostMQResponseQueue=CICSA.REPLY.QUEUE,"
                + "hostMQBridgeType=LSMSG]",
                cicsMQEndpoint.toString());

    }

    /**
     * Instantiate from empty configuration.
     */
    public void testInstantiation2() {
        CicsMQEndpoint cicsMQEndpoint = new CicsMQEndpoint();
        assertEquals(1414, cicsMQEndpoint.getHostIPPort());
        assertEquals("CSQ1", cicsMQEndpoint.getHostMQManager());
        assertEquals("LSMSG", cicsMQEndpoint.getHostMQBridgeType().toString());
        assertEquals("CICS WMQ endpoint:[hostEndpoint=null,"
                + "hostCharset=IBM01140,"
                + "hostUserID=null,"
                + "hostPassword=********,"
                + "hostTraceMode=false,"
                + "connectTimeout=1000,"
                + "receiveTimeout=5000,"
                + "hostConnectionfactoryClass=com.legstar.mq.client.CicsMQConnectionFactory,"
                + "hostAccessStrategy=direct,"
                + "hostConnectionPoolSize=5,"
                + "pooledInvokeTimeout=3000]"
                + "[hostIPAddress=null,"
                + "hostIPPort=1414,"
                + "hostMQManager=CSQ1,"
                + "hostMQChannel=null,"
                + "hostMQRequestQueue=null,"
                + "hostMQResponseQueue=null,"
                + "hostMQBridgeType=LSMSG]",
                cicsMQEndpoint.toString());
    }

    /**
     * Instantiate with IBM MQCIH.
     */
    public void testInstantiationMqcih() {
        CicsMQEndpoint cicsMQEndpoint = AbstractMQConnectionTester.getMqcihEndpoint();
        assertEquals("IBM01140", cicsMQEndpoint.getHostCharset());
        assertEquals("mainframe", cicsMQEndpoint.getHostIPAddress());
        assertEquals(1414, cicsMQEndpoint.getHostIPPort());
        assertEquals("STREAM2", cicsMQEndpoint.getHostPassword());
        assertEquals("CSQ1", cicsMQEndpoint.getHostMQManager());
        assertEquals("CLIENT.TO.CSQ1", cicsMQEndpoint.getHostMQChannel());
        assertEquals("CICS01.BRIDGE.REQUEST.QUEUE", cicsMQEndpoint.getHostMQRequestQueue());
        assertEquals("CICS01.BRIDGE.REPLY.QUEUE", cicsMQEndpoint.getHostMQResponseQueue());
        assertEquals("P390", cicsMQEndpoint.getHostUserID());
        assertEquals("MQCIH", cicsMQEndpoint.getHostMQBridgeType().toString());
        assertEquals("CICS WMQ endpoint:[hostEndpoint=CICSTS23-MQCIH,"
                + "hostCharset=IBM01140,"
                + "hostUserID=P390,"
                + "hostPassword=********,"
                + "hostTraceMode=false,"
                + "connectTimeout=1000,"
                + "receiveTimeout=5000,"
                + "hostConnectionfactoryClass=com.legstar.mq.client.CicsMQConnectionFactory,"
                + "hostAccessStrategy=direct,"
                + "hostConnectionPoolSize=5,"
                + "pooledInvokeTimeout=3000]"
                + "[hostIPAddress=mainframe,"
                + "hostIPPort=1414,"
                + "hostMQManager=CSQ1,"
                + "hostMQChannel=CLIENT.TO.CSQ1,"
                + "hostMQRequestQueue=CICS01.BRIDGE.REQUEST.QUEUE,"
                + "hostMQResponseQueue=CICS01.BRIDGE.REPLY.QUEUE,"
                + "hostMQBridgeType=MQCIH]",
                cicsMQEndpoint.toString());
    }
}
