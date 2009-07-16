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
package com.legstar.csok.test;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.HostEndpoint.AccessStrategy;
import com.legstar.test.client.AbstractConnectionTester;

/**
 * Generic test helper class.
 *
 */
public abstract class AbstractSocketConnectionTester extends AbstractConnectionTester {

    /** A Socket endpoint. */
    private CicsSocketEndpoint mEndpoint;
    
    /** Address of target host. */
    private LegStarAddress mAddress;

    /** A socket connection to a mainframe. */
    private CicsSocket mConnection;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * @return the Address of target host
     */
    public LegStarAddress getAddress() {
        return mAddress;
    }

    /**
     * @return the Socket endpoint
     */
    public CicsSocketEndpoint getEndpoint() {
        return mEndpoint;
    }

    /**
     * @return the host connection
     */
    public CicsSocket getConnection() {
        return mConnection;
    }

    /**
     * Special setup using an endpoint configuration name.
     * @param endpointName endpoint name
     * @throws Exception if setup fails
     */
    public void setUp(final String endpointName) throws Exception {
        if (endpointName.equals("CICSTS23")) {
            mEndpoint = getCicsTs23Endpoint();
        } else if (endpointName.equals("CICSTS23-POOLED")) {
            mEndpoint = getCicsTs23PooledEndpoint();
        } else if (endpointName.equals("CICSTS31")) {
            mEndpoint = getCicsTs31Endpoint();
        } else if (endpointName.equals("CICSTS31-POOLED")) {
            mEndpoint = getCicsTs31PooledEndpoint();
        }
        if (_log.isDebugEnabled()) {
            mEndpoint.setHostTraceMode(true);
        }

        mAddress = new LegStarAddress(endpointName);
        mConnection = new CicsSocket(getName(), getEndpoint());
        mConnection.connect(HOST_PASSWORD);
    }

    /** {@inheritDoc} */
    public void tearDown() throws Exception {
        super.tearDown();
        mConnection.close();
    }

    /**
     * @return and endpoint for CICS TS 2.3
     */
    public static CicsSocketEndpoint getCicsTs23Endpoint() {
        CicsSocketEndpoint endpoint = new CicsSocketEndpoint();
        endpoint.setName("CICSTS23");
        endpoint.setHostIPAddress("mainframe");
        endpoint.setHostIPPort(3011);
        endpoint.setHostUserID("P390");
        endpoint.setHostPassword("STREAM2");
        return endpoint;
    }

    /**
     * @return and endpoint for CICS TS 3.1
     */
    public static CicsSocketEndpoint getCicsTs31Endpoint() {
        CicsSocketEndpoint endpoint = new CicsSocketEndpoint();
        endpoint.setName("CICSTS31");
        endpoint.setHostIPAddress("mainframe");
        endpoint.setHostIPPort(4011);
        endpoint.setHostUserID("P390");
        endpoint.setHostPassword("STREAM2");
        return endpoint;
    }

    /**
     * @return and endpoint for CICS TS 2.3
     */
    public static CicsSocketEndpoint getCicsTs23PooledEndpoint() {
        CicsSocketEndpoint endpoint = getCicsTs23Endpoint();
        endpoint.setName("CICSTS23-POOLED");
        endpoint.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint.setPooledMaxKeepAlive(1000L);
        return endpoint;
    }

    /**
     * @return and endpoint for CICS TS 3.1
     */
    public static CicsSocketEndpoint getCicsTs31PooledEndpoint() {
        CicsSocketEndpoint endpoint = getCicsTs31Endpoint();
        endpoint.setName("CICSTS31-POOLED");
        endpoint.setHostAccessStrategy(AccessStrategy.pooled);
        return endpoint;
    }

    /**
     * @return a pooling engine configuration bean.
     */
    public static PoolingEngineConfig getCicsTs23PoolingEngineConfig() {
        PoolingEngineConfig config = new PoolingEngineConfig();
        List < HostEndpoint > endpoints = new ArrayList < HostEndpoint >();

        endpoints.add(getCicsTs23PooledEndpoint());

        config.setHostEndpoints(endpoints);
        return config;
    }

    /**
     * @return a pooling engine configuration bean.
     */
    public static PoolingEngineConfig getCicsTs31PoolingEngineConfig() {
        PoolingEngineConfig config = new PoolingEngineConfig();
        List < HostEndpoint > endpoints = new ArrayList < HostEndpoint >();

        endpoints.add(getCicsTs31PooledEndpoint());

        config.setHostEndpoints(endpoints);
        return config;
    }
}
