package com.legstar.cixs.jaxws.gen.model;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.jaxws.model.AntBuildJaxws2CixsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;

import junit.framework.TestCase;

public class AntBuildJaxws2CixsModelTest extends TestCase {

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(AntBuildJaxws2CixsModelTest.class);

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/ant";

	public void testMule2CixsBuild() throws Exception {

		AntBuildJaxws2CixsModel model = new AntBuildJaxws2CixsModel();
        model.setProductLocation("/Users/Fady/sandbox/legstar-1.2.0");
        model.setProbeFile(new File("probe.file.tmp"));

        CixsJaxwsService CixsJaxwsService = TestCases.getLsfileae();
        
        model.setCixsJaxwsService(CixsJaxwsService);
        model.setCoxbBinDir(new File("src/test/gen/target/classes"));
        model.setCustBinDir(new File("src/test/gen/target/classes"));
        model.setJaxbBinDir(new File("src/test/gen/target/classes"));
        model.setTargetAntDir(new File("src/test/gen/ant"));
        model.setTargetBinDir(new File("src/test/gen/target/classes"));
        model.setTargetWarDir(new File("${env.CATALINA_HOME}/webapps"));
        model.setTargetWDDDir(new File("src/test/gen/webapps"));
        model.setTargetPropDir(new File("src/test/gen/properties"));
        model.setTargetSrcDir(new File("src/test/gen/java"));

		model.generateBuild(CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));
		
		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("<project basedir=\"/Users/Fady/sandbox/legstar-1.2.0\" default=\"signalSuccess\" name=\"generate-jaxws2Cixs\">"));
		assertTrue(resStr.replace('\\', '/').contains("<pathelement location=\"src/test/gen/target/classes\"/>"));
		assertTrue(resStr.contains("<taskdef name=\"jaxws2cixsgen\""));
		assertTrue(resStr.contains("classname=\"com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator\""));
		assertTrue(resStr.replace('\\', '/').contains("<jaxws2cixsgen targetSrcDir=\"src/test/gen/java\""));
		assertTrue(resStr.replace('\\', '/').contains("targetWDDDir=\"src/test/gen/webapps\""));
        assertTrue(resStr.replace('\\', '/').contains("targetPropDir=\"src/test/gen/properties\""));
        assertTrue(resStr.replace('\\', '/').contains("targetAntDir=\"src/test/gen/ant\""));
        assertTrue(resStr.replace('\\', '/').contains("targetWarDir=\"${env.CATALINA_HOME}/webapps\""));
        assertTrue(resStr.replace('\\', '/').contains("targetBinDir=\"src/test/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("jaxbBinDir=\"src/test/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("coxbBinDir=\"src/test/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("custBinDir=\"src/test/gen/target/classes\""));

		assertTrue(resStr.contains("<cixsJaxwsService name=\"lsfileae\""));
		assertTrue(resStr.contains("packageName=\"com.legstar.test.cixs.lsfileae\""));
		assertTrue(resStr.contains("targetNamespace=\"http://cixs.test.legstar.com/lsfileae\""));
		assertTrue(resStr.contains("<cixsOperation name=\"lsfileae\""));
		assertTrue(resStr.contains("cicsProgramName=\"LSFILEAE\""));
		assertTrue(resStr.contains("jaxbType=\"DfhcommareaType\""));
		assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.lsfileae\""));
		assertTrue(resStr.replace('\\', '/').contains("<mkdir dir=\"src/test/gen/target/classes\"/>"));
		assertTrue(resStr.replace('\\', '/').contains("<javac srcdir=\"src/test/gen/java\""));
	}
	
}
