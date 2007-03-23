package com.legstar.csok.test;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.csok.client.CicsSocketHostConversionException;
import com.legstar.messaging.Address;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.messaging.RequestException;
import com.legstar.config.Constants;

import junit.framework.TestCase;

public class CicsSocketTest extends TestCase {

	private static final String HOST_CHARSET = "IBM01140";
	
	public void testToHostBytes() throws CicsSocketHostConversionException {
		/* Truncation case */
		byte[] hostBytes1 = new byte[8];
		CicsSocket.toHostBytes("cicsUserID", hostBytes1, 0, 8, HOST_CHARSET);
		assertEquals("838983a2e4a28599", Util.toHexString(hostBytes1));
		/* Fill case */
		byte[] hostBytes2 = new byte[8];
		CicsSocket.toHostBytes("cics", hostBytes2, 0, 8, HOST_CHARSET);
		assertEquals("838983a240404040", Util.toHexString(hostBytes2));
		/* Fill in the middle case */
		byte[] hostBytes3 = new byte[12];
		CicsSocket.toHostBytes("cicsuser", hostBytes3, 2, 8, HOST_CHARSET);
		assertEquals("0000838983a2a4a285990000", Util.toHexString(hostBytes3));
	}
	
	public void testTIMFormat() throws CicsSocketHostConversionException {
		byte CIM[] = CicsSocket.formatCIM("cicsUserID", "cicsPassword", "connectionID", true, HOST_CHARSET);
        /*            U S E R I D     P A S S W O R D C O N N E C T I O N I D         T E C */
		assertEquals("838983a2e4a28599838983a2d781a2a2839695958583a3899695c9c440404040f1e2d2", Util.toHexString(CIM));
	}

	public void testFormatMessage() throws CicsSocketHostConversionException {
		byte[] content = Util.toByteArray("838983a2e4a2");
		MessagePart messagePart = new CommareaPart(content);
		byte MPH[] = CicsSocket.formatMPH("LSOKDATA",messagePart, HOST_CHARSET);
        /*            L S O K D A T A   L S O K C O M M A R E A                6*/
		assertEquals("d3e2d6d2c4c1e3c140d3e2d6d2c3d6d4d4c1d9c5c14040404000000006", Util.toHexString(MPH));
	}

	/** Attempt to connect with the wrong USER ID */
	public void testSecurityViolation() {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			endpoint.setHostUserID("TARBOUCH");
			CicsSocket cs = new CicsSocket("testSecurityViolation", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			fail("testSecurityViolation failed");
		} catch (ConnectionException e) {
			assertEquals("com.legstar.messaging.RequestException: SEE06908 The USERID is not known to the external security manager.", e.getMessage().trim());
		} catch (ConfigurationException e) {
			fail("testSecurityViolation failed=" + e);
		}
	}

	/** Close when no connection exist */
	public void testClosePremature() {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testClosePremature", endpoint, 1000, 5000);
			cs.close();
		} catch (RequestException e) {
			fail("testClosePremature failed=" + e);
		} catch (ConfigurationException e) {
			fail("testClosePremature failed=" + e);
		}
	}
	
	/** Connect and immediatly close */
	public void testConnectAndClose() {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testConnectAndClose", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			cs.close();
		} catch (ConnectionException e) {
			fail("testConnectAndClose failed=" + e);
		} catch (RequestException e) {
			fail("testConnectAndClose failed=" + e);
		} catch (ConfigurationException e) {
			fail("testConnectAndClose failed=" + e);
		}
	}
	
	/** Connect and send a message part 
	 * @throws UnsupportedEncodingException */
	public void testConnectSendPart() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testConnectSendPart", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea = new CommareaPart(null);
			inputParts.add(inCommarea);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.close();
		} catch (ConnectionException e) {
			fail("testConnectSendPart failed=" + e);
		} catch (RequestException e) {
			fail("testConnectSendPart failed=" + e);
		} catch (ConfigurationException e) {
			fail("testConnectSendPart failed=" + e);
		}
	}

	/** Connect and send 2 message parts 
	 * @throws UnsupportedEncodingException */
	public void testConnectSend2Parts() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testConnectSendPart", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea = new CommareaPart(Util.toByteArray("F3F4F5"));
			inputParts.add(inCommarea);
			MessagePart inCommarea2 = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inCommarea2);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.close();
		} catch (ConnectionException e) {
			fail("testConnectSendPart failed=" + e);
		} catch (RequestException e) {
			fail("testConnectSendPart failed=" + e);
		} catch (ConfigurationException e) {
			fail("testConnectSendPart failed=" + e);
		}
	}

	/** Send too much input parts
	 * @throws UnsupportedEncodingException */
	public void testSendTooManyParts() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testSendTooManyParts", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			for (int i = 0; i < 11; i++) {
				MessagePart inCommarea = new CommareaPart(null);
				inputParts.add(inCommarea);
			}
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			cs.close();
			fail("testTooManyParts failed=");
		} catch (ConnectionException e) {
			fail("testSendTooManyParts failed=" + e);
		} catch (RequestException e) {
			assertEquals("Too many input message parts.", e.getMessage());
		} catch (ConfigurationException e) {
			fail("testSendTooManyParts failed=" + e);
		}
	}

	/** Test that missing program name is checked correctly
	 * @throws UnsupportedEncodingException */
	public void testMissingProgramName() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testMissingProgramName", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			map.put(Constants.CICS_SYSID_KEY, "CICZ");
			map.put(Constants.CICS_SYNCONRET_KEY, "1");
			map.put(Constants.CICS_TRANSID_KEY, "MIRO");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea = new CommareaPart(null);
			inputParts.add(inCommarea);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			cs.close();
			fail("testMissingProgramName failed=");
		} catch (ConnectionException e) {
			fail("testMissingProgramName failed=" + e);
		} catch (RequestException e) {
			assertEquals("No CICS program name was provided.", e.getMessage());
		} catch (ConfigurationException e) {
			fail("testMissingProgramName failed=" + e);
		}
	}
	
	/** Test that a commarea message part is required
	 * @throws UnsupportedEncodingException */
	public void testMissingCommareapart() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testMissingCommareapart", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			map.put(Constants.CICS_SYSID_KEY, "CICZ");
			map.put(Constants.CICS_SYNCONRET_KEY, "1");
			map.put(Constants.CICS_TRANSID_KEY, "MIRO");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			cs.close();
			fail("testMissingProgramName failed=");
		} catch (ConnectionException e) {
			fail("testMissingCommareapart failed=" + e);
		} catch (RequestException e) {
			assertEquals("No input message part for commarea.", e.getMessage());
		} catch (ConfigurationException e) {
			fail("testMissingCommareapart failed=" + e);
		}
	}
	
	/** Test that a maximum of one message part is accepted for commarea
	 * @throws UnsupportedEncodingException */
	public void testTooManyCommareaparts() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testTooManyCommareaparts", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			map.put(Constants.CICS_SYSID_KEY, "CICZ");
			map.put(Constants.CICS_SYNCONRET_KEY, "1");
			map.put(Constants.CICS_TRANSID_KEY, "MIRO");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea1 = new CommareaPart(null);
			inputParts.add(inCommarea1);
			MessagePart inCommarea2 = new CommareaPart(null);
			inputParts.add(inCommarea2);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			cs.close();
			fail("testTooManyCommareaparts failed=");
		} catch (ConnectionException e) {
			fail("testTooManyCommareaparts failed=" + e);
		} catch (RequestException e) {
			assertEquals("Too many message parts for commarea.", e.getMessage());
		} catch (ConfigurationException e) {
			fail("testTooManyCommareaparts failed=" + e);
		}
	}
	
	/** Test that datalength is correctly checked against length
	 * @throws UnsupportedEncodingException */
	public void testDataLengthGtLength() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testDataLengthGtLength", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "6");
			map.put(Constants.CICS_DATALEN_KEY, "79");
			map.put(Constants.CICS_SYSID_KEY, "CICZ");
			map.put(Constants.CICS_SYNCONRET_KEY, "1");
			map.put(Constants.CICS_TRANSID_KEY, "MIRO");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea1 = new CommareaPart(null);
			inputParts.add(inCommarea1);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			cs.close();
			fail("testDataLengthGtLength failed");
		} catch (ConnectionException e) {
			fail("testDataLengthGtLength failed=" + e);
		} catch (RequestException e) {
			assertEquals("Data length cannot exceed commarea length.", e.getMessage());
		} catch (ConfigurationException e) {
			fail("testDataLengthGtLength failed=" + e);
		}
	}

	/** Send all possible header key/value pairs 
	 * @throws UnsupportedEncodingException */
	public void testSendHeaderCommarea() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testSendHeaderCommarea", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			map.put(Constants.CICS_SYSID_KEY, "CICS");
			map.put(Constants.CICS_SYNCONRET_KEY, "1");
			map.put(Constants.CICS_TRANSID_KEY, "CSMI");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inCommarea);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cs.commitUOW();
			cs.close();
		} catch (ConnectionException e) {
			fail("testSendHeaderCommarea failed=" + e);
		} catch (RequestException e) {
			fail("testSendHeaderCommarea failed=" + e);
		} catch (ConfigurationException e) {
			fail("testSendHeaderCommarea failed=" + e);
		}
	}

	/** Case where the commarea should not be reallocated
	 * @throws UnsupportedEncodingException */
	public void testNoReallocateContent() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testNoReallocateContent", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "3");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea1 = new CommareaPart(Util.toByteArray("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c"));
			inputParts.add(inCommarea1);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cs.keepUOW();
			cs.close();
		} catch (ConnectionException e) {
			fail("testNoReallocateContent failed=" + e);
		} catch (RequestException e) {
			fail("testNoReallocateContent failed=" + e);
		} catch (ConfigurationException e) {
			fail("testNoReallocateContent failed=" + e);
		}
	}

	/** Case where the remote CICS program gets an ASRA abend 
	 * @throws UnsupportedEncodingException */
	public void DoesNotWorktestAsraAbend() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			CicsSocket cs = new CicsSocket("testAsraAbend", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_PROGRAM_KEY, "T1ABEND");
			map.put(Constants.CICS_LENGTH_KEY, "4");
			map.put(Constants.CICS_DATALEN_KEY, "4");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea = new CommareaPart(Util.toByteArray("C1E2D9C1"));
			inputParts.add(inCommarea);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cs.close();
		} catch (ConnectionException e) {
			fail("testAsraAbend failed=" + e);
		} catch (RequestException e) {
			fail("testAsraAbend failed=" + e);
		} catch (ConfigurationException e) {
			fail("testAsraAbend failed=" + e);
		}
	}
	
}
