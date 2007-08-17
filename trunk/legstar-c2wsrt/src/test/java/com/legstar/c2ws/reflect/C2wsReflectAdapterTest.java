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

import java.math.BigDecimal;

import junit.framework.TestCase;

import com.legstar.test.coxb.cultureinfo.CultureInfoReplyType;
import com.legstar.test.coxb.cultureinfo.CultureInfoRequestType;
import com.legstar.c2ws.C2wsConfigurationManager;
import com.legstar.c2ws.CultureInfoCases;
import com.legstar.c2ws.C2wsWSDescriptor;
import com.legstar.c2ws.reflect.C2wsReflectAdapter;
import com.legstar.c2ws.util.C2wsUtil;
import com.legstar.coxb.host.HostData;

public class C2wsReflectAdapterTest extends TestCase {
	
	public void testUnmarshal() throws Exception {
		com.legstar.test.coxb.cultureinfo.ObjectFactory objectFactory =
			new com.legstar.test.coxb.cultureinfo.ObjectFactory();
		byte[] hostBytes = HostData.toByteArray("869960C6D9404040404040404040404040404040404040404040404040404040012564562C");
		C2wsReflectAdapter adapter = new C2wsReflectAdapter();
		CultureInfoRequestType request = (CultureInfoRequestType) adapter.unmarshalReflect(
				objectFactory, CultureInfoRequestType.class, hostBytes);
		assertEquals("fr-FR", request.getCultureCode());
		assertEquals(new BigDecimal("125645.62"), request.getDecimalNumber());
	}
	
	public void testMarshal() throws Exception {
		com.legstar.test.coxb.cultureinfo.ObjectFactory objectFactory =
			new com.legstar.test.coxb.cultureinfo.ObjectFactory();
		C2wsReflectAdapter adapter = new C2wsReflectAdapter();
		byte[] hostBytes = adapter.marshalReflect(
				objectFactory, CultureInfoCases.getResponseJaxbObject());
		assertEquals("9f40404040404040404040404040404040404040404040404040404040404040c699819583854040404040404040404040404040404040404040404040404040c699859583884040404040404040404040404040404040404040404040404040f1f84081a599899340f1f9f9f240f1f87af3f840404040404040404040404040f1f2f54bf6f4f56bf6f240404040404040404040404040404040404040404040859560e4e2404040404040404040404040404040404040404040404040404040e49589a3858440e2a381a385a240404040404040404040404040404040404040c595879389a28840404040404040404040404040404040404040404040404040",
				HostData.toHexString(hostBytes));
	}
	
	public void testInvoke() throws Exception {
		C2wsConfigurationManager c2wsConfigManager = new C2wsConfigurationManager("legstar-c2wsrt-config.xml");
		C2wsWSDescriptor wsd = c2wsConfigManager.getWebServiceDescriptor("CultureInfo");
		C2wsReflectAdapter adapter = new C2wsReflectAdapter();
		byte[] requestBytes = HostData.toByteArray("869960C6D9404040404040404040404040404040404040404040404040404040012564562C");
		byte[] responseBytes = adapter.invoke(wsd, requestBytes);
		CultureInfoReplyType response = CultureInfoCases.getResponseFromHostBytes(responseBytes);
		assertEquals("€", response.getCurrencySymbol());
		assertEquals("France", response.getDisplayCountry());
		assertEquals("français", response.getDisplayLanguage());
		// assertEquals("mardi 17 juillet 2007 10 h 57 CE", response.getFormattedDate());
		assertEquals("125 645,62", response.getFormattedDecimalNumber());
		assertEquals("fr-FR", response.getServerCultureInfo().getCultureCode());
		assertEquals("France", response.getServerCultureInfo().getDisplayCountry());
		assertEquals("français", response.getServerCultureInfo().getDisplayLanguage());
	}
	
	public void testDump() throws Exception {
		byte[] hostBytes = CultureInfoCases.getRequestHostData();
		C2wsUtil.traceData(null, hostBytes, hostBytes.length);
	}

}
