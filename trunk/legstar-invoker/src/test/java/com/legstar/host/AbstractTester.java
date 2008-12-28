package com.legstar.host;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.test.coxb.LsfileaeCases;

import junit.framework.TestCase;

/**
 * Generic helper code for unit tests.
 *
 */
public class AbstractTester extends TestCase {

    /** A valid invoker configuration file. */
    public static final String CONFIG_FILE = "config0.xml";
    
    /** Configuration XPath location for an endpoint. */
    public static final String HOST_ENDPOINT_CFG = "hostEndPoints/hostEndPoint";
    
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
            LegStarRequest request = new LegStarRequest("Request01", address, getLsfileaeRequestMessage());
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
     * Get an endpoint from the configuration file.
     * @param endpointName the endpoint name
     * @return an endpoint
     * @throws ConfigurationException if configuration is wrong
     */
    public static CicsSocketEndpoint getEndpoint(
            final String endpointName) throws ConfigurationException {
        HierarchicalConfiguration sub = Config.loadEndpointConfiguration(CONFIG_FILE, endpointName);
        CicsSocketEndpoint cicsSocketEndpoint = new CicsSocketEndpoint(sub);
        return cicsSocketEndpoint;

    }
}
