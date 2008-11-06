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
package com.legstar.c2ws.servlet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.c2ws.CultureInfoCases;
import com.legstar.c2ws.util.C2wsLog;
import com.legstar.messaging.LegStarMessage;
import com.legstar.test.coxb.cultureinfo.CultureInfoReply;

import junit.framework.TestCase;

public class C2wsProxyTest extends TestCase {

	/** Logger. */
	private static final Log LOG =  LogFactory.getLog(C2wsProxyTest.class);
	
	/* A tomcat server running cultureinfo web service must be up. */
	public void testInvoke() throws Exception {
		MockServletConfig servletConfig = new MockServletConfig();
		servletConfig.addInitParameter(C2wsProxy.HOST_CHARSET_KEY, "IBM01140");
		servletConfig.addInitParameter(C2wsProxy.ADAPTER_CLASSNAME_KEY, "com.legstar.c2ws.reflect.C2wsReflectAdapter");
		
		servletConfig.addInitParameter(C2wsProxy.WSDL_URL_KEY, CultureInfoCases.getWSDescriptor().getWsdlUrl());
		servletConfig.addInitParameter(C2wsProxy.WSDL_TARGET_NAMESPACE_KEY, CultureInfoCases.getWSDescriptor().getWsdlTargetNamespace());
		servletConfig.addInitParameter(C2wsProxy.WSDL_PORT_NAME_KEY, CultureInfoCases.getWSDescriptor().getWsdlPort());
		servletConfig.addInitParameter(C2wsProxy.WSDL_SERVICE_NAME_KEY, CultureInfoCases.getWSDescriptor().getWsdlName());
		servletConfig.addInitParameter(C2wsProxy.REQUEST_JAXB_PACKAGE_NAME_KEY, CultureInfoCases.getWSDescriptor().getRequestElementDescriptor().getJaxbPackageName());
		servletConfig.addInitParameter(C2wsProxy.REQUEST_JAXB_TYPE_KEY, CultureInfoCases.getWSDescriptor().getRequestElementDescriptor().getJaxbType());
		servletConfig.addInitParameter(C2wsProxy.RESPONSE_JAXB_PACKAGE_NAME_KEY, CultureInfoCases.getWSDescriptor().getResponseElementDescriptor().getJaxbPackageName());
		servletConfig.addInitParameter(C2wsProxy.RESPONSE_JAXB_TYPE_KEY, CultureInfoCases.getWSDescriptor().getResponseElementDescriptor().getJaxbType());
		
		C2wsProxy c2wsProxy = new C2wsProxy();
		C2wsLog cxidLog = new C2wsLog(LOG);
		cxidLog.setCorrelationId("TRACE-ID");
		c2wsProxy.init(servletConfig);
		LegStarMessage responseMessage = c2wsProxy.invoke(
				cxidLog, CultureInfoCases.getCultureInfoRequestMessage());
		assertTrue(responseMessage != null);
		assertTrue(responseMessage.getDataParts().size() == 1);
		byte[] responseBytes = responseMessage.getDataParts().get(0).getContent();
		CultureInfoReply response = CultureInfoCases.getResponseFromHostBytes(responseBytes);
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
