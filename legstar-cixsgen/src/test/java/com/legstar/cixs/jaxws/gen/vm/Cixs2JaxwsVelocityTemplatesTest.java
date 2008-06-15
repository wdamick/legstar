package com.legstar.cixs.jaxws.gen.vm;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;

public class Cixs2JaxwsVelocityTemplatesTest extends AbstractTestTemplate {

	/** This generator name. */
	private static final String CIXS_TO_JAXWS_GENERATOR_NAME =
		"LegStar Mainframe to Jaxws generator";

	public void testCobolProgramGeneration() throws Exception {

		CixsJaxwsService model = TestCases.getJvmquery();
		String resStr = genSource(model,
				CIXS_TO_JAXWS_GENERATOR_NAME,
				Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_CLIENT_VLC_TEMPLATE,
				GEN_COBOL_DIR,
				model.getCixsOperations().get(0).getCicsProgramName() + ".cbl");
		
		assertTrue(resStr.contains("       PROGRAM-ID. JVMQUERY."));
		assertTrue(resStr.contains("       77  THIS-TRACE-ID               PIC X(13) VALUE 'JVMQUERY'."));
		assertTrue(resStr.contains("77  C2WS-SERVICE-URI            PIC X(19) VALUE"));
		assertTrue(resStr.contains("http://192.168.0.5/'."));
		assertTrue(resStr.contains("77  C2WS-USERID                 PIC X(5) VALUE"));
		assertTrue(resStr.contains("'alice'."));
		assertTrue(resStr.contains("77  C2WS-PASSWORD               PIC X(12) VALUE"));
		assertTrue(resStr.contains("'inwonderland'."));
		assertTrue(resStr.contains("77  C2WS-SERVICE-NAME           PIC X(8) VALUE"));
		assertTrue(resStr.contains("'jvmquery'."));
		assertTrue(resStr.contains("           05 JvmQueryRequestType."));
		assertTrue(resStr.contains("               10 envVarNames--C PIC 9(9) BINARY."));
		assertTrue(resStr.contains("               10 envVarNames PIC X(32) OCCURS 0"));
		assertTrue(resStr.contains("                   TO 10 DEPENDING ON envVarNames--C."));
		assertTrue(resStr.contains("           05 JvmQueryReplyType."));
		assertTrue(resStr.contains("               10 envVarValues--C PIC 9(9) BINARY."));
		assertTrue(resStr.contains("               10 country PIC X(32)."));
		assertTrue(resStr.contains("               10 currencySymbol PIC X(32)."));
		assertTrue(resStr.contains("               10 envVarValues PIC X(32) OCCURS 0"));
		assertTrue(resStr.contains("                   TO 10 DEPENDING ON envVarValues--C."));
		assertTrue(resStr.contains("               10 formattedDate PIC X(32)."));
		assertTrue(resStr.contains("               10 language PIC X(32)."));
		assertTrue(resStr.contains("'JVMQUERY STARTING ==============================='"));
		assertTrue(resStr.contains("'JVMQUERY STOPPING ==============================='"));
	}
	
	public void testWebXmlGeneration() throws Exception {

		CixsJaxwsService model = TestCases.getCutureInfoModel();
		getParameters().put("hostCharset", "IBM01147");
		String resStr = genSource(model,
				CIXS_TO_JAXWS_GENERATOR_NAME,
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
		assertTrue(resStr.contains("<param-value>GetInfoType</param-value>"));
		assertTrue(resStr.contains("c2ws.requestJaxbPackageName"));
		assertTrue(resStr.contains("<param-value>com.legstar.test.coxb.cultureinfo</param-value>"));
		assertTrue(resStr.contains("c2ws.responseJaxbType"));
		assertTrue(resStr.contains("<param-value>GetInfoResponseType</param-value>"));
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
				CIXS_TO_JAXWS_GENERATOR_NAME,
				Cixs2JaxwsGenerator.SERVICE_ANT_BUILD_WAR_VLC_TEMPLATE,
				GEN_ANT_DIR, 
				"build.xml");
		assertTrue(resStr.contains("<property name=\"service\" value=\"cultureinfo\"/>"));
		assertTrue(resStr.contains("<property name=\"service\" value=\"cultureinfo\"/>"));
		assertTrue(resStr.contains("<delete file=\"/Servers/TOMDev/webapps/cixs-cultureinfo.war\""));
		assertTrue(resStr.contains("<war warfile=\"/Servers/TOMDev/webapps/cixs-cultureinfo.war\""));
		assertTrue(resStr.contains("webxml=\"/Legsem/Legstar/Dev/WebContent/WEB-INF/web.xml\">"));
		assertTrue(resStr.contains("<classes dir=\"target/classes\">"));
		assertTrue(resStr.contains("<include name=\"com/legstar/test/coxb/cultureinfo/*.class\"/>"));
	}
	
}
