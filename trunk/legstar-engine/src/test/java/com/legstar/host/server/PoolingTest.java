/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
import com.legstar.host.server.Engine;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.config.Constants;
import com.legstar.messaging.RequestException;
import com.legstar.work.manager.WorkManagerImpl;

import commonj.work.Work;
import commonj.work.WorkEvent;
import commonj.work.WorkListener;
import commonj.work.WorkManager;

import junit.framework.TestCase;

public class PoolingTest extends TestCase {
	
	private static final String HOST_USERID = "P390";
	private static final String HOST_PASSWORD = "STREAM2";
	private static final int CLIENT_THREADS = 10;

	/** Logger. */
	private static final Log mLog = LogFactory.getLog(PoolingTest.class);
	
	public void testStartStopEngine() throws Exception {
		HierarchicalConfiguration config = Util.getCombinedConfiguration();
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		Thread.sleep(1000L);
		engHandler.stop();
		Thread.sleep(1000L);
	}
	
	public void testScheduleWork() throws Exception {
		HierarchicalConfiguration config = Util.getCombinedConfiguration();
		ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
		WorkManager wm = new WorkManagerImpl(executor);
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		
		LegStarAddress address = new LegStarAddress("TheMainframe");
		address.setHostUserID(HOST_USERID);
		address.setHostPassword(HOST_PASSWORD);
		
		LegStarRequest request = new LegStarRequest("Request01", address, getRequestMessage());
		Client client = new Client(engHandler.getEngine(), "Client01", request);
		wm.schedule(client, new ClientListener());
		
		Thread.sleep(5000L);
		engHandler.stop();
		executor.shutdownNow();

		assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
				Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		
	}

	public void testScheduleWorkInvalidAddress() throws Exception {
		HierarchicalConfiguration config = Util.getCombinedConfiguration();
		ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
		WorkManager wm = new WorkManagerImpl(executor);
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		
		LegStarAddress address = new LegStarAddress("TheIsNoSuchMainframe");
		address.setHostUserID(HOST_USERID);
		address.setHostPassword(HOST_PASSWORD);
		
		LegStarRequest request = new LegStarRequest("Request01", address, getRequestMessage());
		Client client = new Client(engHandler.getEngine(), "Client01", request);
		wm.schedule(client, new ClientListener());
		
		Thread.sleep(5000L);
		engHandler.stop();
		executor.shutdownNow();
		assertTrue(request.getException().getMessage().contains("com.legstar.pool.manager.ConnectionPoolException: org.apache.commons.configuration.ConfigurationException: The requested endpoint:TheIsNoSuchMainframe is not defined."));
		
	}

	public void testScheduleFailingWork() throws Exception {
		HierarchicalConfiguration config = Util.getCombinedConfiguration();
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		
		HashMap < String, Object > map = new HashMap < String, Object >();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "TARATATA");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);

		LegStarAddress address = new LegStarAddress("TheMainframe");
		address.setHostUserID(HOST_USERID);
		address.setHostPassword(HOST_PASSWORD);
		
		LegStarRequest request = new LegStarRequest("testScheduleWork", address, requestMessage);
		synchronized (request) {
			engHandler.getEngine().addRequest(request);
			request.await(3000L, TimeUnit.MILLISECONDS);
		}
		assertTrue(request.getException().getMessage().contains("CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=") );
		assertEquals(null, request.getResponseMessage());
		
		Thread.sleep(1000L);
		engHandler.stop();
	}

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
			LegStarRequest request = new LegStarRequest("Request01", address, getRequestMessage());
			clients[i] = new Client(engHandler.getEngine(), "Client" + Integer.toString(i), request);
			wm.schedule(clients[i], new ClientListener());
			Thread.sleep(20L);
		}
		
		/* Time is needed to process these requests */
		Thread.sleep(10000L);
		engHandler.stop();
		executor.shutdownNow();
		
		for (int i = 0; i < clients.length; i++) {
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					Util.toHexString(clients[i].getRequest().getResponseMessage().getDataParts().get(0).getContent()));
		}
	}
	
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
			LegStarAddress address = ((i % 2) == 0)? address1 : address2;
			LegStarRequest request = new LegStarRequest("Request01", address, getRequestMessage());
			clients[i] = new Client(engHandler.getEngine(), "Client" + new Integer(i).toString(), request);
			wm.schedule(clients[i], new ClientListener());
		}
		
		/* Time is needed to process these requests */
		Thread.sleep(10000L);
		engHandler.stop();
		executor.shutdownNow();
		
		for (int i = 0; i < clients.length; i++) {
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					Util.toHexString(clients[i].getRequest().getResponseMessage().getDataParts().get(0).getContent()));
		}
	}
	
	private LegStarMessage getRequestMessage() throws HeaderPartException {
		HashMap < String, Object > map = new HashMap < String, Object >();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
		return new LegStarMessage(dp, inputParts);
	}
	
	private class Client implements Work {
		
		private Engine mEngine;
		private String mClientID;
		private LegStarRequest mRequest;
		
		public Client(Engine engine, String ID, LegStarRequest request) {
			mEngine = engine;
			mClientID = ID;
			mRequest = request;
		}

		public void run() {
			mLog.debug("Dispatching Request:" + mClientID);
			synchronized (mRequest) {
				try {
					mEngine.addRequest(mRequest);
					mRequest.await(3000L, TimeUnit.MILLISECONDS);
					if (mRequest.getException() != null) {
						throw mRequest.getException();
					} else {
						if (mRequest.getResponseMessage() == null) {
							mLog.error("Timed out waiting for a reply on Request:" + mClientID);
						}
					}
				} catch (InterruptedException e) {
					mLog.error("InterruptedException for Request:" + mClientID + " " + e.getMessage());
					mRequest.setException(e);
					e.printStackTrace();
				} catch (RequestException e) {
					mLog.error("RequestException for Request:" + mClientID + " " + e.getMessage());
					mRequest.setException(e);
					e.printStackTrace();
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}
		
		public LegStarRequest getRequest() {
			return mRequest;
		}
		
		public boolean isDaemon() {
			return false;
		}

		public void release() {
			
		}
	
	}
	private class ClientListener implements WorkListener {
		
		
		/** The work manager has accepted the request. */
		public void workAccepted(WorkEvent arg0) {
			mLog.debug("Client Work accepted.");
		}

		/** The Engine completed its work. */
		public void workCompleted(WorkEvent arg0) {
			if (arg0.getException() != null) {
				mLog.fatal("Client crashed", arg0.getException());
			}
			mLog.debug("Client stopped.");
		}

		/** Manager rejected that work. */
		public void workRejected(WorkEvent arg0) {
			mLog.debug("Client work rejected.");
		}

		/** Work unit has started. */
		public void workStarted(WorkEvent arg0) {
			mLog.debug("Client started.");
		}
			
	}


}
