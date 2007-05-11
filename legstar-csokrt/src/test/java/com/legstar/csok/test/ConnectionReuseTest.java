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
package com.legstar.csok.test;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.messaging.Address;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.messaging.RequestException;
import com.legstar.config.Constants;

import junit.framework.TestCase;

public class ConnectionReuseTest extends TestCase {
	
	public void testReuse()
			throws ConnectionException,
			InterruptedException,
			UnsupportedEncodingException,
			ConfigurationException,
			RequestException, HeaderPartException {
		CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
		CicsSocket cicsSocket = new CicsSocket("testReuseConnection", endpoint, 1000, 5000);
		
		/* Initial connect */
		cicsSocket.connect(null);
		
		/* Wait for the server to timeout (2 secs) */
		Thread.sleep(2500L);
		
		/* At this stage, the server died. Attempt reuse */
		cicsSocket.connectReuse(null);
		
		/* Check if connection is usable */
		HashMap < String, Object > map = new HashMap < String, Object >();
		map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <MessagePart> inputParts = new ArrayList <MessagePart>();
		MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size());
		Address address = new Address("TheMainframe");
		Message requestMessage = new Message(dp, inputParts);
		Request request = new Request("testReuseRequest", address, requestMessage);
		cicsSocket.sendRequest(request);
		cicsSocket.recvResponse(request);
		assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
				Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		
		/* This new attempt should find a Server alive */
		cicsSocket.connectReuse(null);
		
		cicsSocket.sendRequest(request);
		cicsSocket.recvResponse(request);
		assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
				Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		
		cicsSocket.close();
	}

}
