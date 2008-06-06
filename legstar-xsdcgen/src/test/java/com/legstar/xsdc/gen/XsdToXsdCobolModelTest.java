package com.legstar.xsdc.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.net.URI;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenUtil;

import junit.framework.TestCase;

public class XsdToXsdCobolModelTest extends TestCase {


	/** Logger. */
	private static final Log LOG = LogFactory.getLog(XsdToXsdCobolModelTest.class);

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/ant";

	public void testBuilX2sc() throws Exception {

		XsdToXsdCobolModel model = new XsdToXsdCobolModel();

		model.setProductLocation("/Users/Fady/sandbox/legstar-1.2.0");
		model.setInputXsdUri(new URI("http://some.location/wsdl"));
		model.setJaxbTypeClassesSuffix("Type");
		model.setJaxbPackageName("com.legstar.test.coxb.jvmquery");
		model.setNamespace("http://jvmquery.cases.test.xsdc.legstar.com/");
		model.setTargetDir(new File("schema"));
		model.setTargetXsdFileName("jvmquery.xsd");
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
		assertTrue(resStr.contains("<echo message=\"Generating annotated XML schema jvmquery.xsd\" />"));
		assertTrue(resStr.contains("<mkdir dir=\"schema\"/>"));
		assertTrue(resStr.contains("inputXsdUri=\"http://some.location/wsdl\""));
		assertTrue(resStr.contains("jaxbTypeClassesSuffix=\"Type\""));
		assertTrue(resStr.contains("namespace=\"http://jvmquery.cases.test.xsdc.legstar.com/\""));
		assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.jvmquery\""));
		assertTrue(resStr.contains("targetDir=\"schema\""));
		assertTrue(resStr.contains("targetXsdFileName=\"jvmquery.xsd\""));
		assertTrue(resStr.contains("<delete file=\"probe.file.tmp\" quiet=\"true\"/>"));
	}

}
