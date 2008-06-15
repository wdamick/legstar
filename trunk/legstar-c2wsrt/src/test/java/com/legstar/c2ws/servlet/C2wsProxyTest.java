package com.legstar.c2ws.servlet;

import com.legstar.c2ws.CultureInfoCases;
import com.legstar.c2ws.reflect.C2wsReflectAdapter;
import com.legstar.messaging.LegStarMessage;
import com.legstar.test.coxb.cultureinfo.CultureInfoReplyType;

import junit.framework.TestCase;

public class C2wsProxyTest extends TestCase {

	/* A tomcat server running cultureinfo web service must be up. */
	public void testInvoke() throws Exception {
		C2wsProxy c2wsProxy = new C2wsProxy();
		c2wsProxy.setWSDescriptor(CultureInfoCases.getWSDescriptor());
		c2wsProxy.setAdapter(new C2wsReflectAdapter());
		LegStarMessage responseMessage = c2wsProxy.invoke(
				"TRACE-ID", CultureInfoCases.getCultureInfoRequestMessage());
		assertTrue(responseMessage != null);
		assertTrue(responseMessage.getDataParts().size() == 1);
		byte[] responseBytes = responseMessage.getDataParts().get(0).getContent();
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
	
}
