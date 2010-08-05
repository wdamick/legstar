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
package com.legstar.mock.client;

import java.util.HashMap;
import java.util.Map;

import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;

import junit.framework.TestCase;

/**
 * Test the MockConnection.
 *
 */
public class MockConnectionTest extends TestCase {

    /**
     * Get a Mock connection via the factory.
     */
    public void testInstantiate() {
        try {
            LegStarConnection connection = getConnection();
            assertTrue(null != connection);

        } catch (Exception e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Try executing a program through the Mock.
     */
    public void testExecute() {
        try {
            LegStarConnection connection = getConnection();
            LegStarRequest request = getLsfileaeRequest100();
            connection.recvResponse(request);
            assertTrue(null != request.getResponseMessage());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(), HostData.toHexString(
                    request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

    /**
     * @return a new instance of a mock connection
     */
    private LegStarConnection getConnection() {
        try {
            MockConnectionFactory mockConnectionFactory = new MockConnectionFactory();
            return mockConnectionFactory.createConnection("connectionID", null, null);
        } catch (ConnectionException e) {
            fail(e.getMessage());
            return null;
        }
    }

    /**
     * Create a request with a header built from a properties map.
     * @param map properties map for header creation
     * @return a legstar request
     * @throws RequestException if unable to create request
     */
    public LegStarRequest getRequest(final Map < String, Object > map) throws RequestException {
        try {
            LegStarMessage requestMessage = new LegStarMessage();
            requestMessage.getHeaderPart().setKeyValues(map);
            return new LegStarRequest(getName(), null, requestMessage);
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        }
    }

    /**
     * Create a request for LSFILEAE customer 100.
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public LegStarRequest getLsfileaeRequest100() throws RequestException {
        Map < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        
        LegStarRequest request = getRequest(map);
        request.getRequestMessage().addDataPart(new CommareaPart(
                HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
        return request;

    }


}
