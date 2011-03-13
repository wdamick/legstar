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

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.HostEndpoint.AccessStrategy;
import com.legstar.messaging.LegStarAddress;
import com.legstar.mq.client.CicsMQEndpoint.HostMQBridgeType;
import com.legstar.test.connection.client.AbstractConnectionTester;

/**
 * Generic test helper class.
 * 
 */
public abstract class AbstractMQConnectionTester extends AbstractConnectionTester {

    /** An endpoint. */
    private CicsMQEndpoint mEndpoint;

    /** Address of target host. */
    private LegStarAddress mAddress;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * @return the Address of target host
     */
    public LegStarAddress getAddress() {
        return mAddress;
    }

    /**
     * @return the Http endpoint
     */
    public CicsMQEndpoint getEndpoint() {
        return mEndpoint;
    }

    /**
     * Special setup using an endpoint configuration name.
     * 
     * @param endpointName endpoint name
     * @throws Exception if setup fails
     */
    public void setUp(final String endpointName) throws Exception {
        if (endpointName.equals("CICSTS23-LSMSG")) {
            mEndpoint = getLsmsgEndpoint();
        } else if (endpointName.equals("CICSTS23-MQCIH")) {
            mEndpoint = getMqcihEndpoint();
        }
        if (_log.isDebugEnabled()) {
            mEndpoint.setHostTraceMode(true);
        }

        mAddress = new LegStarAddress(endpointName);
    }

    /**
     * @return and endpoint for LegStar messaging
     */
    public static CicsMQEndpoint getLsmsgEndpoint() {
        CicsMQEndpoint endpoint = new CicsMQEndpoint();
        endpoint.setName("CICSTS23-LSMSG");
        endpoint.setInitialContextFactory("org.osjava.sj.SimpleContextFactory");
        Properties jndiProperties = new Properties();
        jndiProperties.put("org.osjava.sj.root",
                "src/test/resources/simple-jndi");
        endpoint.setJndiProperties(jndiProperties);
        endpoint.setJndiConnectionFactoryName("ConnectionFactory");
        endpoint.setHostUserID("P390");
        endpoint.setHostPassword("STREAM2");
        endpoint.setJndiRequestQueueName("CicsARequestQueue");
        endpoint.setJndiReplyQueueName("CicsAReplyQueue");
        return endpoint;
    }

    /**
     * @return and endpoint for MQCIH messaging
     */
    public static CicsMQEndpoint getMqcihEndpoint() {
        CicsMQEndpoint endpoint = new CicsMQEndpoint();
        endpoint.setName("CICSTS23-MQCIH");
        endpoint.setInitialContextFactory("org.osjava.sj.SimpleContextFactory");
        Properties jndiProperties = new Properties();
        jndiProperties.put("org.osjava.sj.root",
                "src/test/resources/simple-jndi");
        endpoint.setJndiProperties(jndiProperties);
        endpoint.setJndiConnectionFactoryName("ConnectionFactory");
        endpoint.setJndiRequestQueueName("Cics01BridgeRequestQueue");
        endpoint.setJndiReplyQueueName("Cics01BridgeReplyQueue");
        endpoint.setHostUserID("P390");
        endpoint.setHostPassword("STREAM2");
        endpoint.setHostMQBridgeType(HostMQBridgeType.MQCIH);
        return endpoint;
    }

    /**
     * @return and endpoint for LegStar messaging
     */
    public static CicsMQEndpoint getLsmsgPooledEndpoint() {
        CicsMQEndpoint endpoint = getLsmsgEndpoint();
        endpoint.setName("CICSTS23-LSMSG-POOLED");
        endpoint.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint.setPooledMaxIdleTime(-1);
        return endpoint;
    }

    /**
     * @return and endpoint for LegStar messaging
     */
    public static CicsMQEndpoint getMqcihPooledEndpoint() {
        CicsMQEndpoint endpoint = getMqcihEndpoint();
        endpoint.setName("CICSTS23-MQCIH-POOLED");
        endpoint.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint.setPooledMaxIdleTime(-1);
        return endpoint;
    }

    /**
     * @return a pooling engine configuration bean.
     */
    public static PoolingEngineConfig getLsmsgPoolingEngineConfig() {
        PoolingEngineConfig config = new PoolingEngineConfig();
        List < HostEndpoint > endpoints = new ArrayList < HostEndpoint >();

        endpoints.add(getLsmsgPooledEndpoint());

        config.setHostEndpoints(endpoints);
        return config;
    }

    /**
     * @return a pooling engine configuration bean.
     */
    public static PoolingEngineConfig getMqcihPoolingEngineConfig() {
        PoolingEngineConfig config = new PoolingEngineConfig();
        List < HostEndpoint > endpoints = new ArrayList < HostEndpoint >();

        endpoints.add(getMqcihPooledEndpoint());

        config.setHostEndpoints(endpoints);
        return config;
    }

}
