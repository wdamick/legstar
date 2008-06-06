package com.legstar.schemagen.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenUtil;
import com.legstar.schemagen.CobolToXsdCobolModel;

import junit.framework.TestCase;

public class CobolToXsdCobolModelTest extends TestCase {


	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CobolToXsdCobolModelTest.class);

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/ant";

	public void testBuilc2sc() throws Exception {

		CobolToXsdCobolModel model = new CobolToXsdCobolModel();

		model.setProductLocation("/Users/Fady/sandbox/legstar-1.2.0");
		model.setSourceCobolFilePath("src/main/resources/cobol/LSFILEAE.CBL");
		model.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
		model.setNamespace("http://lsfileae.cases.test.schemagen.legstar.com/");
		model.setTargetDir(new File("src/test/gen/schema"));
		model.setTargetXsdFileName("lsfileae.xsd");
		model.setProbeFile(new File("probe.file.tmp"));
		
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
		assertTrue(resStr.contains("<project basedir=\"/Users/Fady/sandbox/legstar-1.2.0\" default=\"signalSuccess\" name=\"generate-XSD\">"));
		assertTrue(resStr.contains("<echo message=\"Generating annotated XML schema lsfileae.xsd\" />"));
		assertTrue(resStr.contains("namespace=\"http://lsfileae.cases.test.schemagen.legstar.com/\""));
		assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.lsfileae\""));
		assertTrue(resStr.contains("sourceCobolFilePath=\"src/main/resources/cobol/LSFILEAE.CBL\""));
		assertTrue(resStr.contains("targetDir=\"src\\test\\gen\\schema\""));
		assertTrue(resStr.contains("targetXsdFileName=\"lsfileae.xsd\""));
		assertTrue(resStr.contains("<delete file=\"probe.file.tmp\" quiet=\"true\"/>"));
	}

}
