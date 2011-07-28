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
        CicsMQEndpoint cicsMQEndpoint = AbstractMQConnectionTester
                .getLsmsgEndpoint();
        assertEquals("IBM01140", cicsMQEndpoint.getHostCharset());
        assertEquals("org.osjava.sj.SimpleContextFactory",
                cicsMQEndpoint.getInitialContextFactory());
        assertEquals("ConnectionFactory",
                cicsMQEndpoint.getJndiConnectionFactoryName());
        assertEquals(null, cicsMQEndpoint.getJndiProviderURL());
        assertEquals(null, cicsMQEndpoint.getJndiUrlPkgPrefixes());
        assertEquals("org.osjava.sj.root=src/test/resources/simple-jndi",
                cicsMQEndpoint.getJndiProperties().toString());
        assertEquals("CicsARequestQueue",
                cicsMQEndpoint.getJndiRequestQueueName());
        assertEquals("CicsAReplyQueue", cicsMQEndpoint.getJndiReplyQueueName());
        assertEquals("P390", cicsMQEndpoint.getHostUserID());
        assertEquals("STREAM2", cicsMQEndpoint.getHostPassword());
        assertEquals("LSMSG", cicsMQEndpoint.getHostMQBridgeType().toString());
        assertEquals(
                "CICS WMQ endpoint:[hostEndpoint=CICSTS23-LSMSG,"
                        + "hostCharset=IBM01140,"
                        + "hostUserID=P390,"
                        + "hostPassword=********,"
                        + "hostTraceMode=false,"
                        + "connectTimeout=1000,"
                        + "receiveTimeout=5000,"
                        + "hostConnectionfactoryClass=com.legstar.mq.client.CicsMQConnectionFactory,"
                        + "hostAccessStrategy=direct,"
                        + "hostConnectionPoolSize=5,"
                        + "pooledInvokeTimeout=3000,"
                        + "pooledMaxIdleTime=-1,"
                        + "pooledMaxIdleTimeCheckPeriod=-1]"
                        + "[initialContextFactory=org.osjava.sj.SimpleContextFactory,"
                        + "jndiProviderURL=null,"
                        + "jndiUrlPkgPrefixes=null,"
                        + "jndiProperties={org.osjava.sj.root=src/test/resources/simple-jndi},"
                        + "jndiConnectionFactoryName=ConnectionFactory,"
                        + "jndiRequestQueueName=CicsARequestQueue,"
                        + "jndiReplyQueueName=CicsAReplyQueue,"
                        + "hostMQBridgeType=LSMSG]", cicsMQEndpoint.toString());

    }

    /**
     * Instantiate from empty configuration.
     */
    public void testInstantiation2() {
        CicsMQEndpoint cicsMQEndpoint = new CicsMQEndpoint();
        assertEquals(null, cicsMQEndpoint.getInitialContextFactory());
        assertEquals(null, cicsMQEndpoint.getJndiConnectionFactoryName());
        assertEquals(null, cicsMQEndpoint.getJndiProviderURL());
        assertEquals(null, cicsMQEndpoint.getJndiUrlPkgPrefixes());
        assertEquals(null, cicsMQEndpoint.getJndiProperties());
        assertEquals(null, cicsMQEndpoint.getJndiRequestQueueName());
        assertEquals(null, cicsMQEndpoint.getJndiReplyQueueName());
        assertEquals("LSMSG", cicsMQEndpoint.getHostMQBridgeType().toString());
        assertEquals(
                "CICS WMQ endpoint:[hostEndpoint=null,"
                        + "hostCharset=IBM01140,"
                        + "hostUserID=null,"
                        + "hostPassword=********,"
                        + "hostTraceMode=false,"
                        + "connectTimeout=1000,"
                        + "receiveTimeout=5000,"
                        + "hostConnectionfactoryClass=com.legstar.mq.client.CicsMQConnectionFactory,"
                        + "hostAccessStrategy=direct,"
                        + "hostConnectionPoolSize=5,"
                        + "pooledInvokeTimeout=3000," + "pooledMaxIdleTime=-1,"
                        + "pooledMaxIdleTimeCheckPeriod=-1]"
                        + "[initialContextFactory=null,"
                        + "jndiProviderURL=null," + "jndiUrlPkgPrefixes=null,"
                        + "jndiProperties={null},"
                        + "jndiConnectionFactoryName=null,"
                        + "jndiRequestQueueName=null,"
                        + "jndiReplyQueueName=null,"
                        + "hostMQBridgeType=LSMSG]", cicsMQEndpoint.toString());
    }

    /**
     * Instantiate with IBM MQCIH.
     */
    public void testInstantiationMqcih() {
        CicsMQEndpoint cicsMQEndpoint = AbstractMQConnectionTester
                .getMqcihEndpoint();
        assertEquals("org.osjava.sj.SimpleContextFactory",
                cicsMQEndpoint.getInitialContextFactory());
        assertEquals("ConnectionFactory",
                cicsMQEndpoint.getJndiConnectionFactoryName());
        assertEquals(null, cicsMQEndpoint.getJndiProviderURL());
        assertEquals(null, cicsMQEndpoint.getJndiUrlPkgPrefixes());
        assertEquals("org.osjava.sj.root=src/test/resources/simple-jndi",
                cicsMQEndpoint.getJndiProperties().toString());
        assertEquals("Cics01BridgeRequestQueue",
                cicsMQEndpoint.getJndiRequestQueueName());
        assertEquals("Cics01BridgeReplyQueue",
                cicsMQEndpoint.getJndiReplyQueueName());
        assertEquals("P390", cicsMQEndpoint.getHostUserID());
        assertEquals("STREAM2", cicsMQEndpoint.getHostPassword());
        assertEquals("MQCIH", cicsMQEndpoint.getHostMQBridgeType().toString());
        assertEquals(
                "CICS WMQ endpoint:[hostEndpoint=CICSTS23-MQCIH,"
                        + "hostCharset=IBM01140,"
                        + "hostUserID=P390,"
                        + "hostPassword=********,"
                        + "hostTraceMode=false,"
                        + "connectTimeout=1000,"
                        + "receiveTimeout=5000,"
                        + "hostConnectionfactoryClass=com.legstar.mq.client.CicsMQConnectionFactory,"
                        + "hostAccessStrategy=direct,"
                        + "hostConnectionPoolSize=5,"
                        + "pooledInvokeTimeout=3000,"
                        + "pooledMaxIdleTime=-1,"
                        + "pooledMaxIdleTimeCheckPeriod=-1]"
                        + "[initialContextFactory=org.osjava.sj.SimpleContextFactory,"
                        + "jndiProviderURL=null,"
                        + "jndiUrlPkgPrefixes=null,"
                        + "jndiProperties={org.osjava.sj.root=src/test/resources/simple-jndi},"
                        + "jndiConnectionFactoryName=ConnectionFactory,"
                        + "jndiRequestQueueName=Cics01BridgeRequestQueue,"
                        + "jndiReplyQueueName=Cics01BridgeReplyQueue,"
                        + "hostMQBridgeType=MQCIH]", cicsMQEndpoint.toString());
    }
}
