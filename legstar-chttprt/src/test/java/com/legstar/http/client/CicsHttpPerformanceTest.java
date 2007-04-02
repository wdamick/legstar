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

public class CicsHttpPerformanceTest extends TestCase {
	private static final String CONFIG_FILE = "config.xml";
	
	private CicsHttpConnectionFactory mfactory;
	
	protected void setUp() throws Exception {
		super.setUp();
		HierarchicalConfiguration endpointConfig =
			Config.loadEndpointConfiguration(
					Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
		mfactory =
			new CicsHttpConnectionFactory(endpointConfig);
	}
	/* with CICS TS 2.3, there is no support for HTTP 1.1. HTTPClient will not keep
	 * the session alive. */
	public void testSend2Requests() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);
			cicsHttp.setConnectTimeout(2000);
			cicsHttp.connect(null); // let config pick the password
			Request request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (UnsupportedEncodingException e) {
			fail("testSend2Requests failed " + e);
		} catch (ConnectionException e) {
			fail("testSend2Requests failed " + e);
		} catch (RequestException e) {
			fail("testSend2Requests failed " + e);
		}
	}
	
	private Request createStdRequest() throws UnsupportedEncodingException {
		Address address = new Address("TheMainframe");
		HashMap < String, String > map = new HashMap < String, String >();
		map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <MessagePart> inputParts = new ArrayList <MessagePart>();
		MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
		Message requestMessage = new Message(dp, inputParts);
		Request request = new Request("Request01", address, requestMessage);
		return request;
		
	}


}
