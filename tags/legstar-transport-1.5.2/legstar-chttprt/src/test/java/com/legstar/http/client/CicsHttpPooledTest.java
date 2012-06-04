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
package com.legstar.http.client;

import java.util.concurrent.TimeUnit;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.coxb.host.HostData;
import com.legstar.host.server.EngineHandler;
import com.legstar.host.server.EngineHolder;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.connection.client.AbstractConnectionTester;
import com.legstar.test.coxb.LsfileaeCases;

import junit.framework.TestCase;

/**
 * Test HTTP connection in pooling condition.
 *
 */
public class CicsHttpPooledTest extends TestCase {
    
    /** Time out (in milliseconds) for invoke. */
    private static final long DEFAULT_INVOKE_TIMEOUT_MSEC = 30000L;

    /** Configuration bean used for this test session.*/
    private PoolingEngineConfig _config = AbstractHttpConnectionTester.getCicsTs31PoolingEngineConfig();
   
    /** {@inheritDoc} */
    public void setUp() throws Exception {
        EngineHandler serverHandler = new EngineHandler(_config);
        serverHandler.init();
    }
    
    /** {@inheritDoc} */
    public void tearDown() {
        EngineHolder.stop();
    }

    /**
     * Picks up connections from the pool one by one.
     * @throws Exception if something goes wrong
     */
    public void testCyclePool() throws Exception {
        for (int i = 0; i < _config.getHostEndpoints().get(0).getHostConnectionPoolSize(); i++) {
            invokeLsfileae();
        }
    }

    /**
     * Perform request/response to program LSFILEAE.
     * @throws Exception if something goes wrong
     */
    public void invokeLsfileae() throws Exception {
        LegStarRequest request = AbstractConnectionTester.getLsfileaeRequest100(
                new LegStarAddress(_config.getHostEndpoints().get(0).getName()));
        EngineHolder.getEngine().addRequest(request);
        request.await(DEFAULT_INVOKE_TIMEOUT_MSEC, TimeUnit.MILLISECONDS);
        if (request.getException() != null) {
            throw request.getException();
        } else {
            if (request.getResponseMessage() == null) {
                throw new RequestException(
                        "Timed out waiting for a response for Request:"
                        + request.getID());
            }
        }
        assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
        assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
    }

}
