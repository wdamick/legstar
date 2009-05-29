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
package com.legstar.host.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.config.Constants;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.work.manager.WorkManagerImpl;

import commonj.work.Work;
import commonj.work.WorkEvent;
import commonj.work.WorkListener;
import commonj.work.WorkManager;

import junit.framework.TestCase;

/**
 * Test the pooling engine.
 *
 */
public class PoolingTest extends TestCase {

    /** Mainframe user ID. */
    private static final String HOST_USERID = "P390";
    
    /** Mainframe password. */
    private static final String HOST_PASSWORD = "STREAM2";
    
    /** Number of client threads. */
    private static final int CLIENT_THREADS = 10;

    /** Logger. */
    private final Log _log = LogFactory.getLog(PoolingTest.class);

    /**
     * Try start/stop.
     * @throws Exception if failure
     */
    public void testStartStopEngine() throws Exception {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        EngineHandler engHandler = new EngineHandler(config);
        engHandler.init();
        Thread.sleep(1000L);
        engHandler.stop();
        Thread.sleep(1000L);
    }

    /**
     * Schedule some basic work.
     * @throws Exception if failure
     */
    public void testScheduleWork() throws Exception {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
        WorkManager wm = new WorkManagerImpl(executor);
        EngineHandler engHandler = new EngineHandler(config);
        engHandler.init();

        LegStarAddress address = new LegStarAddress("TheMainframe");
        address.setHostUserID(HOST_USERID);
        address.setHostPassword(HOST_PASSWORD);

        LegStarRequest request = new LegStarRequest("Request01", address, getLsfileaeRequestMessage());
        Client client = new Client(engHandler.getEngine(), "Client01", request);
        wm.schedule(client, new ClientListener());

        Thread.sleep(5000L);
        engHandler.stop();
        executor.shutdownNow();

        assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));

    }

    /**
     * Address a request to an invalid address.
     * @throws Exception if test fails
     */
    public void testScheduleWorkInvalidAddress() throws Exception {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
        WorkManager wm = new WorkManagerImpl(executor);
        EngineHandler engHandler = new EngineHandler(config);
        engHandler.init();

        LegStarAddress address = new LegStarAddress("ThereIsNoSuchMainframe");
        address.setHostUserID(HOST_USERID);
        address.setHostPassword(HOST_PASSWORD);

        LegStarRequest request = new LegStarRequest("Request01", address, getLsfileaeRequestMessage());
        Client client = new Client(engHandler.getEngine(), "Client01", request);
        wm.schedule(client, new ClientListener());

        Thread.sleep(5000L);
        engHandler.stop();
        executor.shutdownNow();
        assertTrue(request.getException().getMessage().contains("com.legstar.pool.manager.ConnectionPoolException:"
        + " org.apache.commons.configuration.ConfigurationException:"
        + " The requested endpoint:ThereIsNoSuchMainframe is not defined."));

    }

    /**
     * Schedule some work that is guaranteed to fail.
     * @throws Exception if test fails
     */
    public void testScheduleFailingWork() throws Exception {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        EngineHandler engHandler = new EngineHandler(config);
        engHandler.init();

        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "TARATATA");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        List < LegStarMessagePart > inputParts = new ArrayList < LegStarMessagePart >();
        LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
        inputParts.add(inCommarea);
        LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
        LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);

        LegStarAddress address = new LegStarAddress("TheMainframe");
        address.setHostUserID(HOST_USERID);
        address.setHostPassword(HOST_PASSWORD);

        LegStarRequest request = new LegStarRequest("testScheduleFailingWork", address, requestMessage);
        synchronized (request) {
            engHandler.getEngine().addRequest(request);
            request.await(3000L, TimeUnit.MILLISECONDS);
        }
        assertTrue(request.getException().getMessage().contains(
                "CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2="));
        assertEquals(null, request.getResponseMessage());

        Thread.sleep(1000L);
        engHandler.stop();
    }

    /**
     * Schedule multiple simultaneous work units.
     * @throws Exception if test fails
     */
    public void testScheduleMultipleWork() throws Exception {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
        WorkManager wm = new WorkManagerImpl(executor);
        EngineHandler engHandler = new EngineHandler(config);
        engHandler.init();

        LegStarAddress address = new LegStarAddress("TheMainframe");
        address.setHostUserID(HOST_USERID);
        address.setHostPassword(HOST_PASSWORD);

        Client[] clients = new Client[3];
        for (int i = 0; i < clients.length; i++) {
            LegStarRequest request = new LegStarRequest("Request01", address, getLsfileaeRequestMessage());
            clients[i] = new Client(engHandler.getEngine(), "Client" + Integer.toString(i), request);
            wm.schedule(clients[i], new ClientListener());
            Thread.sleep(20L);
        }

        /* Time is needed to process these requests */
        Thread.sleep(10000L);
        engHandler.stop();
        executor.shutdownNow();

        for (int i = 0; i < clients.length; i++) {
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    Util.toHexString(clients[i].getRequest().getResponseMessage().getDataParts().get(0).getContent()));
        }
    }

    /**
     * Test work dispatched in more than one pool.
     * @throws Exception if test fails
     */
    public void testScheduleMultiplePools() throws Exception {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
        WorkManager wm = new WorkManagerImpl(executor);
        EngineHandler engHandler = new EngineHandler(config);
        engHandler.init();

        LegStarAddress address1 = new LegStarAddress("TheMainframe");
        address1.setHostUserID(HOST_USERID);
        address1.setHostPassword(HOST_PASSWORD);

        LegStarAddress address2 = new LegStarAddress("TheMainframe");
        address2.setHostUserID("IBMUSER");
        address2.setHostPassword(HOST_PASSWORD);

        Client[] clients = new Client[3];
        for (int i = 0; i < clients.length; i++) {
            LegStarAddress address = ((i % 2) == 0) ? address1 : address2;
            LegStarRequest request = new LegStarRequest("Request01", address, getLsfileaeRequestMessage());
            clients[i] = new Client(engHandler.getEngine(), "Client" + new Integer(i).toString(), request);
            wm.schedule(clients[i], new ClientListener());
        }

        /* Time is needed to process these requests */
        Thread.sleep(10000L);
        engHandler.stop();
        executor.shutdownNow();

        for (int i = 0; i < clients.length; i++) {
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    Util.toHexString(clients[i].getRequest().getResponseMessage().getDataParts().get(0).getContent()));
        }
    }

    /**
     * @return a formatted LegStarMessage requesting execution of LSFILEAE.
     * @throws HeaderPartException if formatting fails
     */
    private LegStarMessage getLsfileaeRequestMessage() throws HeaderPartException {
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        List < LegStarMessagePart > inputParts = new ArrayList < LegStarMessagePart >();
        LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
        inputParts.add(inCommarea);
        LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
        return new LegStarMessage(dp, inputParts);
    }

    /**
     * A Client unit of work class.
     *
     */
    private class Client implements Work {

        /** Reference to pooling engine. */
        private Engine mEngine;
        
        /** Client unique ID. */
        private String mClientID;
        
        /** The request being processed. */
        private LegStarRequest mRequest;

        /**
         * Constructor.
         * @param engine the scheduling engine
         * @param clientID the client ID
         * @param request the request
         */
        public Client(final Engine engine, final String clientID, final LegStarRequest request) {
            mEngine = engine;
            mClientID = clientID;
            mRequest = request;
        }

        /** {@inheritDoc} */
        public void run() {
            _log.debug("Dispatching Request:" + mClientID);
            synchronized (mRequest) {
                try {
                    mEngine.addRequest(mRequest);
                    mRequest.await(3000L, TimeUnit.MILLISECONDS);
                    if (mRequest.getException() != null) {
                        throw mRequest.getException();
                    } else {
                        if (mRequest.getResponseMessage() == null) {
                            _log.error("Timed out waiting for a reply on Request:" + mClientID);
                        }
                    }
                } catch (InterruptedException e) {
                    _log.error("InterruptedException for Request:" + mClientID + " " + e.getMessage());
                    mRequest.setException(e);
                    e.printStackTrace();
                } catch (RequestException e) {
                    _log.error("RequestException for Request:" + mClientID + " " + e.getMessage());
                    mRequest.setException(e);
                    e.printStackTrace();
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }

        /**
         * @return the request
         */
        public LegStarRequest getRequest() {
            return mRequest;
        }

        /**  {@inheritDoc} */
        public boolean isDaemon() {
            return false;
        }

        /**  {@inheritDoc} */
        public void release() {

        }

    }

    /**
     * Client listening class.
     *
     */
    private class ClientListener implements WorkListener {


        /** The work manager has accepted the request.
         * @param arg0 work event*/
        public void workAccepted(final WorkEvent arg0) {
            _log.debug("Client Work accepted.");
        }

        /** The Engine completed its work.
         * @param arg0 work event */
        public void workCompleted(final WorkEvent arg0) {
            if (arg0.getException() != null) {
                _log.fatal("Client crashed", arg0.getException());
            }
            _log.debug("Client stopped.");
        }

        /** Manager rejected that work.
         * @param arg0 work event */
        public void workRejected(final WorkEvent arg0) {
            _log.debug("Client work rejected.");
        }

        /** Work unit has started.
         * @param arg0 work event */
        public void workStarted(final WorkEvent arg0) {
            _log.debug("Client started.");
        }

    }

}
