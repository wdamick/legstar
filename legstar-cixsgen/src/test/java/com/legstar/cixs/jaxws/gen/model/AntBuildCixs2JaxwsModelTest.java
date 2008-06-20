package com.legstar.cixs.jaxws.gen.model;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.jaxws.model.AntBuildCixs2JaxwsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;

import junit.framework.TestCase;

public class AntBuildCixs2JaxwsModelTest extends TestCase {

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(AntBuildCixs2JaxwsModelTest.class);

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/ant";

	public void testCixs2JaxwsBuild() throws Exception {

		AntBuildCixs2JaxwsModel antModel = new AntBuildCixs2JaxwsModel();
        antModel.setProductLocation("/Users/Fady/sandbox/legstar-1.2.0");
        antModel.setProbeFile(new File("probe.file.tmp"));

        CixsJaxwsService CixsJaxwsService = TestCases.getCutureInfoModel();
        
        antModel.setCixsJaxwsService(CixsJaxwsService);
        antModel.setCoxbBinDir(new File("src/test/gen/target/classes"));
        antModel.setCustBinDir(new File("src/test/gen/target/classes"));
        antModel.setJaxbBinDir(new File("src/test/gen/target/classes"));
        antModel.setTargetAntDir(new File("src/test/gen/ant"));
        antModel.setTargetWarDir(new File("${env.CATALINA_HOME}/webapps"));
        antModel.setTargetWDDDir(new File("src/test/gen/webapps"));
        antModel.setTargetCobolDir(new File("src/test/gen/cobol"));
        antModel.setHostCharset("IBM01147");

		antModel.generateBuild(CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));
		
		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("<project basedir=\"/Users/Fady/sandbox/legstar-1.2.0\" default=\"signalSuccess\" name=\"generate-cixs2jaxws\">"));
		assertTrue(resStr.replace('\\', '/').contains("<pathelement location=\"src/test/gen/target/classes\"/>"));
		assertTrue(resStr.contains("<taskdef name=\"cixs2jaxwsgen\""));
		assertTrue(resStr.contains("classname=\"com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator\""));
		assertTrue(resStr.replace('\\', '/').contains("<cixs2jaxwsgen"));
        assertTrue(resStr.replace('\\', '/').contains("targetAntDir=\"src/test/gen/ant\""));
		assertTrue(resStr.replace('\\', '/').contains("targetWDDDir=\"src/test/gen/webapps\""));
        assertTrue(resStr.replace('\\', '/').contains("targetWarDir=\"${env.CATALINA_HOME}/webapps\""));
        assertTrue(resStr.replace('\\', '/').contains("targetCobolDir=\"src/test/gen/cobol\""));
        assertTrue(resStr.replace('\\', '/').contains("jaxbBinDir=\"src/test/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("coxbBinDir=\"src/test/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("custBinDir=\"src/test/gen/target/classes\""));
		assertTrue(resStr.contains("hostCharset=\"IBM01147\""));

		assertTrue(resStr.contains("<cixsJaxwsService name=\"cultureinfo\""));
		assertTrue(resStr.contains("wsdlUrl=\"http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl\""));
		assertTrue(resStr.contains("wsdlPortName=\"CultureInfoImplPort\""));
		assertTrue(resStr.contains("wsdlServiceName=\"CultureInfoImplService\""));
		assertTrue(resStr.contains("targetNamespace=\"http://cultureinfo.cases.test.xsdc.legstar.com/\""));
		assertTrue(resStr.contains("serviceURI=\"http://locahost:8080/c2ws-cultureinfo/cultureinfoProxy\""));
		assertTrue(resStr.contains("<cixsOperation name=\"cultureinfo\""));
		assertTrue(resStr.contains("cicsProgramName=\"CULTUREI\""));
		assertTrue(resStr.contains("jaxbType=\"GetInfoType\""));
		assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.cultureinfo\""));
		assertTrue(resStr.contains("jaxbType=\"GetInfoResponseType\""));
		assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.cultureinfo\""));
	}
	
}
