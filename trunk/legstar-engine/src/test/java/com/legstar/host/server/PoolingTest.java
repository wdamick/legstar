/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.host.server;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 
import com.legstar.host.server.Engine;
import com.legstar.messaging.Address;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.config.Constants;
import com.legstar.messaging.RequestException;
import com.legstar.work.manager.LsWorkManager;

import commonj.work.Work;
import commonj.work.WorkEvent;
import commonj.work.WorkListener;
import commonj.work.WorkManager;

import junit.framework.TestCase;

public class PoolingTest extends TestCase {
	
	private static final String HOST_USERID = "P390";
	private static final String HOST_PASSWORD = "STREAM2";
	private static final int CLIENT_THREADS = 10;
	private static final String CONFIG_FILE = "config.xml";

	/** Logger. */
	private static final Log mLog = LogFactory.getLog(PoolingTest.class);
	
	public void testStartStopEngine() throws Exception {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		Thread.sleep(1000L);
		engHandler.stop();
		Thread.sleep(1000L);
	}
	
	public void testScheduleWork() throws Exception {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
		WorkManager wm = new LsWorkManager(executor);
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		
		Address address = new Address("TheMainframe");
		address.setHostUserID(HOST_USERID);
		address.setHostPassword(HOST_PASSWORD);
		
		Request request = new Request("Request01", address, getRequestMessage());
		Client client = new Client(engHandler.getEngine(), "Client01", request);
		wm.schedule(client, new ClientListener());
		
		Thread.sleep(5000L);
		engHandler.stop();
		executor.shutdownNow();

		assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
				Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		
	}

	public void testScheduleWorkInvalidAddress() throws Exception {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
		WorkManager wm = new LsWorkManager(executor);
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		
		Address address = new Address("TheIsNoSuchMainframe");
		address.setHostUserID(HOST_USERID);
		address.setHostPassword(HOST_PASSWORD);
		
		Request request = new Request("Request01", address, getRequestMessage());
		Client client = new Client(engHandler.getEngine(), "Client01", request);
		wm.schedule(client, new ClientListener());
		
		Thread.sleep(5000L);
		engHandler.stop();
		executor.shutdownNow();
		assertTrue(request.getException().getMessage().contains("com.legstar.pool.manager.ConnectionPoolException: org.apache.commons.configuration.ConfigurationException: The requested endpoint:TheIsNoSuchMainframe is not defined."));
		
	}

	public void testScheduleFailingWork() throws Exception {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		
		HashMap < String, String > map = new HashMap < String, String >();
		map.put(Constants.CICS_PROGRAM_KEY, "TARATATA");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <MessagePart> inputParts = new ArrayList <MessagePart>();
		MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
		Message requestMessage = new Message(dp, inputParts);

		Address address = new Address("TheMainframe");
		address.setHostUserID(HOST_USERID);
		address.setHostPassword(HOST_PASSWORD);
		
		Request request = new Request("testScheduleWork", address, requestMessage);
		synchronized (request) {
			engHandler.getEngine().addRequest(request);
			request.wait(3000L);
		}
		assertEquals("CICS command=LINK failed, resp=PGMIDERR, resp2=3", request.getException().getMessage());
		assertEquals(null, request.getResponseMessage());
		
		Thread.sleep(1000L);
		engHandler.stop();
	}

	public void testScheduleMultipleWork() throws Exception {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
		WorkManager wm = new LsWorkManager(executor);
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();
		
		Address address = new Address("TheMainframe");
		address.setHostUserID(HOST_USERID);
		address.setHostPassword(HOST_PASSWORD);
		
		Client[] clients = new Client[3];
		for (int i = 0; i < clients.length; i++) {
			Request request = new Request("Request01", address, getRequestMessage());
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
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		ExecutorService executor = Executors.newFixedThreadPool(CLIENT_THREADS);
		WorkManager wm = new LsWorkManager(executor);
		EngineHandler engHandler = new EngineHandler(config);
		engHandler.init();

		Address address1 = new Address("TheMainframe");
		address1.setHostUserID(HOST_USERID);
		address1.setHostPassword(HOST_PASSWORD);
		
		Address address2 = new Address("TheMainframe");
		address2.setHostUserID("IBMUSER");
		address2.setHostPassword(HOST_PASSWORD);
		
		Client[] clients = new Client[3];
		for (int i = 0; i < clients.length; i++) {
			Address address = ((i % 2) == 0)? address1 : address2;
			Request request = new Request("Request01", address, getRequestMessage());
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
	
	private Message getRequestMessage() throws UnsupportedEncodingException {
		HashMap < String, String > map = new HashMap < String, String >();
		map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <MessagePart> inputParts = new ArrayList <MessagePart>();
		MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
		return new Message(dp, inputParts);
	}
	
	private class Client implements Work {
		
		private Engine mEngine;
		private String mClientID;
		private Request mRequest;
		
		public Client(Engine engine, String ID, Request request) {
			mEngine = engine;
			mClientID = ID;
			mRequest = request;
		}

		public void run() {
			mLog.debug("Dispatching Request:" + mClientID);
			synchronized (mRequest) {
				try {
					mEngine.addRequest(mRequest);
					mRequest.wait(3000L);
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
		
		public Request getRequest() {
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
