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
import java.util.List;

import junit.framework.TestCase;

import com.legstar.test.coxb.MSNSearch.ResultType;
import com.legstar.test.coxb.MSNSearch.Search;
import com.legstar.test.coxb.MSNSearch.SearchResponse;
import com.legstar.test.coxb.MSNSearch.SourceResponseType;
import com.legstar.test.coxb.cultureinfo.CultureInfoReplyType;
import com.legstar.test.coxb.cultureinfo.CultureInfoParametersType;
import com.legstar.c2ws.CultureInfoCases;
import com.legstar.c2ws.C2wsWSDescriptor;
import com.legstar.c2ws.MSNSearchCases;
import com.legstar.c2ws.reflect.C2wsReflectAdapter;
import com.legstar.c2ws.util.C2wsUtil;
import com.legstar.coxb.host.HostData;

public class C2wsReflectAdapterTest extends TestCase {
	
	public void testUnmarshal() throws Exception {
		com.legstar.test.coxb.cultureinfo.ObjectFactory objectFactory =
			new com.legstar.test.coxb.cultureinfo.ObjectFactory();
		byte[] hostBytes = HostData.toByteArray("869960C6D9404040404040404040404040404040404040404040404040404040012564562C");
		C2wsReflectAdapter adapter = new C2wsReflectAdapter();
		CultureInfoParametersType request = (CultureInfoParametersType) adapter.unmarshalReflect(
				objectFactory, CultureInfoParametersType.class, hostBytes);
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
	
	/* A tomcat server running cultureinfo web service must be up. */
	public void testInvoke() throws Exception {
		C2wsWSDescriptor wsd = CultureInfoCases.getWSDescriptor();
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

	public void testMarshalMSNSearch() throws Exception {
		com.legstar.test.coxb.MSNSearch.ObjectFactory objectFactory =
			new com.legstar.test.coxb.MSNSearch.ObjectFactory();
		C2wsReflectAdapter adapter = new C2wsReflectAdapter();
		byte[] hostBytes = adapter.marshalReflect(
				objectFactory, MSNSearchCases.getResponseJaxbObject());
		assertEquals("000000000000000100000001e685824040404040404040404040404040404040404040404040404040404040000000000000001ef2f340d3858787858440e2a381998689a2884040404040404040404040404040e2a3a484a840a285a3a24081958440a283898595838540878986a3a24086999688a3a3977a6161a6a6a64ba28389859583859481939360a4a2814b83969461f24040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404000000000000000000000000000000000000000000000000040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040400000000000000000000000000000000000000000000000004040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040400000000000000000000000004040404040404040404040404040404040404040404040404040404040404040000000000000000000000000",
				HostData.toHexString(hostBytes));
	}
	
	public void testUnmarshalMSNSearch() throws Exception {
		com.legstar.test.coxb.MSNSearch.ObjectFactory objectFactory =
			new com.legstar.test.coxb.MSNSearch.ObjectFactory();
		byte[] hostBytes = HostData.toByteArray("0000000000000000000000000000000000000001f5f5f8f8c3f3c1c3c5f9f4f9f3f1f5c2f3c5c3c1c1c4c4c1f9f0f8f6f1f1c2c4c6f5c4f8c4f5c1c19389a58540a28581998388404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040859560e4e24040404040404040404040d49684859981a385404040404040404040404040404040404040404040404040000000000000000000000000000000000000000000000000e68582404040404040404040404040404040404040404040404040404040404000000000000000014040404040404040404040404040404040404040404040404040404040404040");
		C2wsReflectAdapter adapter = new C2wsReflectAdapter();
		Search request = (Search) adapter.unmarshalReflect(
				objectFactory, Search.class, hostBytes);
		assertEquals("5588C3ACE949315B3ECAADDA908611BDF5D8D5AA", request.getRequest().getAppID());
		assertEquals("en-US", request.getRequest().getCultureInfo());
		assertEquals("live search", request.getRequest().getQuery());
		assertEquals("MODERATE", request.getRequest().getSafeSearch().toString());
	}
	
	/* MSNSearch must be available. */
	public void testInvokeMSNSearch() throws Exception {
		C2wsWSDescriptor wsd = MSNSearchCases.getWSDescriptor();
		C2wsReflectAdapter adapter = new C2wsReflectAdapter();
		byte[] requestBytes = MSNSearchCases.getRequestHostData();
		byte[] responseBytes = adapter.invoke(wsd, requestBytes);
		SearchResponse response = MSNSearchCases.getResponseFromHostBytes(responseBytes);
		List < SourceResponseType > sourceResponses = response.getResponse().getResponses().getSourceResponse();
		assertEquals(1, sourceResponses.size());
		assertTrue(sourceResponses.get(0).getTotal() > 0);
		List < ResultType > results = sourceResponses.get(0).getResults().getResult();
		assertEquals(1, results.size());
		assertEquals("Live Search", results.get(0).getTitle());
		assertEquals("Find exactly what you are lookin", results.get(0).getDescription());
		assertEquals("http://www.live.com/", results.get(0).getUrl());
	}
	
	public void testDumpMSNSearch() throws Exception {
		byte[] hostBytes = MSNSearchCases.getRequestHostData();
		assertEquals("0000000000000000000000000000000000000001f5f5f8f8c3f3c1c3c5f9f4f9f3f1f5c2f3c5c3c1c1c4c4c1f9f0f8f6f1f1c2c4c6f5c4f8c4f5c1c19389a58540a28581998388404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040859560e4e24040404040404040404040d49684859981a385404040404040404040404040404040404040404040404040000000000000000000000000000000000000000000000000e68582404040404040404040404040404040404040404040404040404040404000000000000000014040404040404040404040404040404040404040404040404040404040404040",
				HostData.toHexString(hostBytes));
		C2wsUtil.traceData(null, hostBytes, hostBytes.length);
	}
}
