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
		C2wsConfigurationManager c2wsConfigManager = new C2wsConfigurationManager("legstar-c2wsrt-config.xml");
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
