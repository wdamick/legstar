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
package com.legstar.csok.test;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;
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
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
		LegStarAddress address = new LegStarAddress("TheMainframe");
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
		LegStarRequest request = new LegStarRequest("testReuseRequest", address, requestMessage);
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
