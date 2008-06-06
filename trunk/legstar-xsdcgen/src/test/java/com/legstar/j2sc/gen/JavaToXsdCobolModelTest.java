package com.legstar.j2sc.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenUtil;

import junit.framework.TestCase;

public class JavaToXsdCobolModelTest extends TestCase {


	/** Logger. */
	private static final Log LOG = LogFactory.getLog(JavaToXsdCobolModelTest.class);

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/ant";

	public void testBuilJ2sc() throws Exception {

		JavaToXsdCobolModel model = new JavaToXsdCobolModel();

		model.setProductLocation("/Users/Fady/sandbox/legstar-1.2.0");
		List < String > classNames = new ArrayList < String >();
		classNames.add("com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest");
		classNames.add("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply");
		model.setClassNames(classNames);
		List < String > pathElementLocations = new ArrayList < String >();
		pathElementLocations.add("/Users/pat/therat/cobol.jar");
		pathElementLocations.add("/Users/pat/thecat");
		model.setPathElementLocations(pathElementLocations);
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
		assertTrue(resStr.contains("<pathelement location=\"/Users/pat/therat/cobol.jar\"/>"));
		assertTrue(resStr.contains("<pathelement location=\"/Users/pat/thecat\"/>"));
		assertTrue(resStr.contains("<echo message=\"Generating annotated XML schema jvmquery.xsd\" />"));
		assertTrue(resStr.contains("<mkdir dir=\"schema\"/>"));
		assertTrue(resStr.contains("namespace=\"http://jvmquery.cases.test.xsdc.legstar.com/\""));
		assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.jvmquery\""));
		assertTrue(resStr.contains("targetDir=\"schema\""));
		assertTrue(resStr.contains("targetXsdFileName=\"jvmquery.xsd\""));
		assertTrue(resStr.contains("<rootClass name=\"com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest\"/>"));
		assertTrue(resStr.contains("<rootClass name=\"com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply\"/>"));
		assertTrue(resStr.contains("<delete file=\"probe.file.tmp\" quiet=\"true\"/>"));
	}

}
