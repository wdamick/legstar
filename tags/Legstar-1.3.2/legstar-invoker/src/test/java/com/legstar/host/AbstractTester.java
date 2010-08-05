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
package com.legstar.host;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.legstar.config.Constants;
import com.legstar.config.PoolingEngineConfig;
import com.legstar.coxb.host.HostData;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.HostEndpoint.AccessStrategy;
import com.legstar.mock.client.MockEndpoint;
import com.legstar.test.coxb.LsfileaeCases;

import junit.framework.TestCase;

/**
 * Generic helper code for unit tests.
 *
 */
public class AbstractTester extends TestCase {

    
    /** A complete configuration file. */
    public static final String CONFIG_FILE = "config0.xml";
    
    /** Mainframe user ID. */
    public static final String HOST_USERID = "P390";

    /** Mainframe password. */
    public static final String HOST_PASSWORD = "STREAM2";

    /** 
     * Create a request to execute LSFILEAE.
     * @return a formatted request
     * @throws HostAccessStrategyException if request cannot be created
     */
    public LegStarRequest createLsfileaeRequest() throws HostAccessStrategyException {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            LegStarRequest request = new LegStarRequest("Lsfileae100", address, getLsfileaeRequestMessage());
            return request;
        } catch (HeaderPartException e) {
            throw new HostAccessStrategyException(e);
        }
    }

    /** 
     * Create a long request ( over 4 secs).
     * @return a long request
     * @throws HostAccessStrategyException if request cannot be created
     */
    public LegStarRequest createT1sleeptRequest() throws HostAccessStrategyException {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            LegStarRequest request = new LegStarRequest("Request01", address, getT1sleeptRequestMessage());
            return request;
        } catch (HeaderPartException e) {
            throw new HostAccessStrategyException(e);
        }
    }

    /**
     * @return a formatted LegStarMessage requesting execution of LSFILEAE.
     * @throws HeaderPartException if formatting fails
     */
    public static LegStarMessage getLsfileaeRequestMessage() throws HeaderPartException {
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        List < LegStarMessagePart > inputParts = new ArrayList < LegStarMessagePart >();
        LegStarMessagePart inCommarea = new CommareaPart(HostData.toByteArray(LsfileaeCases.getHostBytesHexReply100()));
        inputParts.add(inCommarea);
        LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
        return new LegStarMessage(dp, inputParts);
    }

    /**
     * @return a formatted LegStarMessage requesting execution of LSFILEAE.
     * @throws HeaderPartException if formatting fails
     */
    public LegStarMessage getT1sleeptRequestMessage() throws HeaderPartException {
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1SLEEPT");
        map.put(Constants.CICS_LENGTH_KEY, "39");
        map.put(Constants.CICS_DATALEN_KEY, "8");
        List < LegStarMessagePart > inputParts = new ArrayList < LegStarMessagePart >();
        LegStarMessagePart inCommarea = new CommareaPart(HostData.toByteArray("f0f0f0f0f0f0f0f4"));
        inputParts.add(inCommarea);
        LegStarHeaderPart dp;
        dp = new LegStarHeaderPart(map, inputParts.size());
        return new LegStarMessage(dp, inputParts);
    }

    /**
     * @return a standard host endpoint
     */
    public HostEndpoint getStandardHostEndpoint() {
        HostEndpoint endpoint = new MockEndpoint();
        endpoint.setName("TheMainframe");
        endpoint.setHostConnectionfactoryClass("com.legstar.mock.client.MockConnectionFactory");
        endpoint.setHostTraceMode(true);
        //        endpoint.setConnectTimeout(timeout);
        //        endpoint.setHostAccessStrategy(accessStrategy);
        //        endpoint.setHostCharset(hostCharset);
        //        endpoint.setHostConnectionPoolSize(connectionPoolSize);
        //        endpoint.setHostPassword(hostPassword);
        //        endpoint.setHostUserID(hostUserID);
        //        endpoint.setReceiveTimeout(timeout);
        return endpoint;
    }

    /**
     * @return a pooled host endpoint
     */
    public HostEndpoint getPooledHostEndpoint() {
        HostEndpoint endpoint = new MockEndpoint();
        endpoint.setName("TheMainframe");
        endpoint.setHostConnectionfactoryClass("com.legstar.mock.client.MockConnectionFactory");
        endpoint.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint.setPooledInvokeTimeout(2000);
        return endpoint;
    }

    /**
     * @return a pooling engine configuration bean
     */
    public PoolingEngineConfig getPoolingEngineConfig() {
        PoolingEngineConfig config = new PoolingEngineConfig();
        List < HostEndpoint > endpoints = new ArrayList < HostEndpoint >();

        HostEndpoint endpoint1 = new MockEndpoint();
        endpoint1.setName("TheMainframe");
        endpoint1.setHostConnectionfactoryClass("com.legstar.mock.client.MockConnectionFactory");
        endpoint1.setHostAccessStrategy(AccessStrategy.direct);
        endpoints.add(endpoint1);

        HostEndpoint endpoint2 = new MockEndpoint();
        endpoint2.setName("TheOtherMainframe");
        endpoint2.setHostConnectionfactoryClass("com.legstar.mock.client.MockConnectionFactory");
        endpoint2.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint2.setHostConnectionPoolSize(2);
        endpoint2.setPooledInvokeTimeout(2000);
        endpoints.add(endpoint2);
        config.setHostEndpoints(endpoints);
        return config;
    }
}
