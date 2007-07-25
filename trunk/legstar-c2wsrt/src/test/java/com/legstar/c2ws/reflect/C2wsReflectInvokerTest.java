package com.legstar.c2ws.reflect;

import java.util.ArrayList;
import java.util.List;

import com.legstar.c2ws.C2wsConfigurationManager;
import com.legstar.c2ws.C2wsInvoker;
import com.legstar.c2ws.CultureInfoCases;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;

import junit.framework.TestCase;

public class C2wsReflectInvokerTest extends TestCase {
	
	public void testGetServiceName() throws Exception {
		HeaderPart headerPart = new HeaderPart();
		headerPart.setJsonString("{\"ServiceName\":\"whatIamLookingFor\"}");
		Message requestMessage = new Message();
		requestMessage.setHeaderPart(headerPart);
		
		assertEquals("whatIamLookingFor", C2wsInvoker.getServiceName(requestMessage));
	}
	
	public void testInvoke() throws Exception {
		C2wsConfigurationManager c2wsConfigManager = new C2wsConfigurationManager("c2wsconfig.xml");
		C2wsInvoker c2wsInvoker = new C2wsInvoker(null, c2wsConfigManager);
		Message responseMessage = c2wsInvoker.invoke(getCultureInfoRequestMessage());
		assertTrue(responseMessage != null);
	}
	
	private Message getCultureInfoRequestMessage() throws Exception {
		List <MessagePart> dataParts = new ArrayList <MessagePart>();
		MessagePart inCommarea = new CommareaPart(CultureInfoCases.getRequestHostData());
		dataParts.add(inCommarea);
		HeaderPart headerPart = new HeaderPart();
		headerPart.setJsonString("{\"ServiceName\":\"CultureInfo\"}");
		return new Message(headerPart, dataParts);
	}
	
}
