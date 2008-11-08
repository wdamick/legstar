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
package com.legstar.cixs.jaxws.gen.vm;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;

public class Cixs2JaxwsVelocityTemplatesTest extends AbstractTestTemplate {

	public void testWebXmlGeneration() throws Exception {

		CixsJaxwsService model = TestCases.getCutureInfoModel();
		getParameters().put("hostCharset", "IBM01147");
		String resStr = genSource(model,
				Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
				Cixs2JaxwsGenerator.SERVICE_WEB_XML_VLC_TEMPLATE,
				GEN_WDD_DIR,
				"web.xml");
		assertTrue(resStr.contains("<display-name>cultureinfoProxy</display-name>"));
		assertTrue(resStr.contains("<description>cultureinfoProxy is a proxy for Web Service cultureinfo</description>"));
		assertTrue(resStr.contains("<servlet-class>com.legstar.c2ws.servlet.C2wsProxy</servlet-class>"));
		assertTrue(resStr.contains("<param-name>c2ws.wsdlUrl</param-name>"));
		assertTrue(resStr.contains("<param-value>http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl</param-value>"));
		assertTrue(resStr.contains("c2ws.targetNamespace"));
		assertTrue(resStr.contains("<param-value>http://cultureinfo.cases.test.xsdc.legstar.com/</param-value>"));
		assertTrue(resStr.contains("c2ws.wsdlPortName"));
		assertTrue(resStr.contains("<param-value>CultureInfoImplPort</param-value>"));
		assertTrue(resStr.contains("c2ws.wsdlServiceName"));
		assertTrue(resStr.contains("<param-value>CultureInfoImplService</param-value>"));
		assertTrue(resStr.contains("c2ws.requestJaxbType"));
		assertTrue(resStr.contains("<param-value>GetInfo</param-value>"));
		assertTrue(resStr.contains("c2ws.requestJaxbPackageName"));
		assertTrue(resStr.contains("<param-value>com.legstar.test.coxb.cultureinfo</param-value>"));
		assertTrue(resStr.contains("c2ws.responseJaxbType"));
		assertTrue(resStr.contains("<param-value>GetInfoResponse</param-value>"));
		assertTrue(resStr.contains("c2ws.responseJaxbPackageName"));
		assertTrue(resStr.contains("<param-value>com.legstar.test.coxb.cultureinfo</param-value>"));
		assertTrue(resStr.contains("c2ws.hostCharset"));
		assertTrue(resStr.contains("IBM01147"));
		assertTrue(resStr.contains("<url-pattern>/cultureinfoProxy</url-pattern>"));
	}
		
	public void testAntWarBuildGeneration() throws Exception {

		getParameters().put("jaxbBinDir", "target/classes");
		getParameters().put("targetWarDir", "/Servers/TOMDev/webapps");
		getParameters().put("targetWDDDir", "/Legsem/Legstar/Dev/WebContent/WEB-INF");
		
		CixsJaxwsService model = TestCases.getCutureInfoModel();
		String resStr = genSource(model,
				Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
				Cixs2JaxwsGenerator.SERVICE_ANT_BUILD_WAR_VLC_TEMPLATE,
				GEN_ANT_DIR, 
				"build.xml");
		assertTrue(resStr.contains("<property name=\"service\" value=\"cultureinfo\"/>"));
		assertTrue(resStr.contains("<property name=\"service\" value=\"cultureinfo\"/>"));
		assertTrue(resStr.contains("<delete file=\"/Servers/TOMDev/webapps/c2ws-cultureinfo.war\""));
		assertTrue(resStr.contains("<war warfile=\"/Servers/TOMDev/webapps/c2ws-cultureinfo.war\""));
		assertTrue(resStr.contains("webxml=\"/Legsem/Legstar/Dev/WebContent/WEB-INF/web.xml\">"));
		assertTrue(resStr.contains("<classes dir=\"target/classes\">"));
		assertTrue(resStr.contains("<include name=\"com/legstar/test/coxb/cultureinfo/*.class\"/>"));
	}
	
}
