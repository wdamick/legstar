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
package com.legstar.http.client;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.config.Constants;
import com.legstar.messaging.Address;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.messaging.RequestException;

import junit.framework.TestCase;

public class VolumeTest extends TestCase {
	private static final String CONFIG_FILE = "config.xml";
	
	private CicsHttpConnectionFactory mfactory;

	private static final int MAX_ITERATIONS = 1;
	private static final String HOST_CHARSET = "IBM01140";
	
	
	protected void setUp() throws Exception {
		super.setUp();
		HierarchicalConfiguration endpointConfig =
			Config.loadEndpointConfiguration(
					Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
		mfactory =
			new CicsHttpConnectionFactory(endpointConfig);
	}
	
	/** Single thread iterates thru large data requests
	 * @throws UnsupportedEncodingException */
	public void testSingleIterateVolume() throws UnsupportedEncodingException {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testSingleIterateVolume", address);
			cicsHttp.setReceiveTimeout(10000);
			cicsHttp.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_PROGRAM_KEY, "T1VOLUME");
			map.put(Constants.CICS_LENGTH_KEY, "32767");
			map.put(Constants.CICS_DATALEN_KEY, "32767");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			byte[] content = new byte[32767];
			byte[] startEC = Util.toByteArray("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7");
			byte[] endEC = Util.toByteArray("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3");
			System.arraycopy(startEC, 0, content, 0, 16);
			System.arraycopy(endEC, 0, content, 32751, 16);
			MessagePart inCommarea1 = new CommareaPart(content);
			inputParts.add(inCommarea1);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), HOST_CHARSET);
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("testSingleIterateVolume", address, requestMessage);
			for (int i = 0; i < MAX_ITERATIONS; i++) {
				cicsHttp.sendRequest(request);
				cicsHttp.recvResponse(request);
				cicsHttp.keepUOW();
				System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 0, startEC, 0, 16);
				System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 32751, endEC, 0, 16);
				assertEquals("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3", Util.toHexString(startEC));
				assertEquals("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7", Util.toHexString(endEC));
			}
			cicsHttp.close();
		} catch (ConnectionException e) {
			fail("testSingleIterateVolume failed=" + e);
		} catch (RequestException e) {
			e.printStackTrace();
			fail("testSingleIterateVolume failed=" + e);
		}
	}

}
