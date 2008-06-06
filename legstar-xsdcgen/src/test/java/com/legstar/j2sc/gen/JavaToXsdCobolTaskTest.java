package com.legstar.j2sc.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;

import com.legstar.j2sc.gen.JavaToXsdCobolTask;

import junit.framework.TestCase;

public class JavaToXsdCobolTaskTest extends TestCase {
	
	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(JavaToXsdCobolTaskTest.class);
	
	private static final String TARGET_SCHEMA_DIR = "src/test/gen/schema";

	public void testInputValidations() throws Exception {
		JavaToXsdCobolTask task = new JavaToXsdCobolTask();
		try {
			task.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("You must provide a target directory", e.getMessage());
		}
		task.setTargetDir(new File("bidule"));
		try {
			task.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("Directory bidule does not exist", e.getMessage());
		}
		task.setTargetDir(new File(TARGET_SCHEMA_DIR));
		try {
			task.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("You must provide a target xsd file name", e.getMessage());
		}
		task.setTargetXsdFileName("JVMQuery.xsd");
		try {
			task.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("You must specify an output XML schema namespace", e.getMessage());
		}
		task.setNamespace("http://legstar.test");
		try {
			task.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("You must provide at least one class name", e.getMessage());
		}
		task.addRootClass("JVMQuery");
		try {
			task.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("java.lang.ClassNotFoundException: JVMQuery", e.getMessage());
		}
	}
	
	public void testGenerationFromPojo() throws Exception {
		final String targetFile = "JVMQueryReply.xsd";
		JavaToXsdCobolTask task = new JavaToXsdCobolTask();
		task.setTargetDir(new File(TARGET_SCHEMA_DIR));
		task.setTargetXsdFileName(targetFile);
		task.addRootClass("com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest");
		task.addRootClass("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply");
		task.setNamespace("http://legstar.com");
		try {
			task.execute();
			String result = getSource(TARGET_SCHEMA_DIR, targetFile);
			assertTrue(result.contains("targetNamespace=\"http://legstar.com\""));
			assertTrue(result.contains("<xs:element name=\"jvmQueryRequest\" type=\"jvmQueryRequest\">"));
			assertTrue(result.contains("cb:cobolElement byteLength=\"32\" cobolName=\"envVarNames\" levelNumber=\"3\" maxOccurs=\"10\" minOccurs=\"0\" picture=\"X(32)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"));
			assertTrue(result.contains("<xs:element name=\"jvmQueryReply\" type=\"jvmQueryReply\">"));
			assertTrue(result.contains("<cb:cobolElement byteLength=\"32\" cobolName=\"country\" levelNumber=\"3\" picture=\"X(32)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"));
		} catch (BuildException e) {
			assertEquals("java.lang.ClassNotFoundException: JVMQuery", e.getMessage());
		}
	}
	
	public void testComplexTypeToJavaMapping() throws Exception {
		final String targetFile = "JVMQueryReply.xsd";
		JavaToXsdCobolTask task = new JavaToXsdCobolTask();
		task.setNamespace("http://legstar.com");
		Class <? >[] classes = {Class.forName("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply")};
		Map < String, String > complexTypeToJavaClassMap =
			new HashMap < String, String >();
		
		try {
			task.generateSchema(classes, new File(TARGET_SCHEMA_DIR + targetFile), complexTypeToJavaClassMap);
			String result = getSource(TARGET_SCHEMA_DIR, targetFile);
			assertTrue(result.contains("targetNamespace=\"http://legstar.com\""));
			assertTrue(result.contains("<xs:element name=\"jvmQueryReply\" type=\"jvmQueryReply\">"));
			assertTrue(complexTypeToJavaClassMap.size() == 1);
			assertEquals("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply", complexTypeToJavaClassMap.get("jvmQueryReply"));
		} catch (BuildException e) {
			assertEquals("java.lang.ClassNotFoundException: JVMQuery", e.getMessage());
		}
	}

	public void testGenerationFromAnnotated() throws Exception {
		final String targetFile = "CultureInfoRequest.xsd";
		JavaToXsdCobolTask task = new JavaToXsdCobolTask();
		task.setTargetDir(new File(TARGET_SCHEMA_DIR));
		task.setTargetXsdFileName(targetFile);
		task.addRootClass("com.legstar.xsdc.test.cases.cultureinfo.CultureInfoRequest");
		task.setNamespace("http://legstar.com");
		try {
			task.execute();
			String result = getSource(TARGET_SCHEMA_DIR, targetFile);
			assertTrue(result.contains("targetNamespace=\"http://legstar.com\""));
			assertTrue(result.contains("<xs:element name=\"cultureInfoParameters\" type=\"cultureInfoParameters\">"));
			assertTrue(result.contains("<cb:cobolType javaClassName=\"com.legstar.xsdc.test.cases.cultureinfo.CultureInfoRequest\"/>"));
			assertTrue(result.contains("<cb:cobolElement cobolName=\"cultureInfoParameters\" levelNumber=\"1\" type=\"GROUP_ITEM\"/>"));
		} catch (BuildException e) {
			assertEquals("java.lang.ClassNotFoundException: JVMQuery", e.getMessage());
		}
	}
	
	public void testGenerationFromAggregate() throws Exception {
		final String targetFile = "CultureInfoReply.xsd";
		JavaToXsdCobolTask task = new JavaToXsdCobolTask();
		task.setTargetDir(new File(TARGET_SCHEMA_DIR));
		task.setTargetXsdFileName(targetFile);
		task.addRootClass("com.legstar.xsdc.test.cases.cultureinfo.CultureInfoReply");
		task.setNamespace("http://legstar.com");
		try {
			task.execute();
			String result = getSource(TARGET_SCHEMA_DIR, targetFile);
			assertTrue(result.contains("targetNamespace=\"http://legstar.com\""));
			assertTrue(result.contains("<xs:complexType name=\"cultureInfoReply\">"));
			assertTrue(result.contains("<cb:cobolElement byteLength=\"32\" cobolName=\"currencySymbol\" levelNumber=\"3\" picture=\"X(32)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"));
			assertTrue(result.contains("<xs:element minOccurs=\"0\" name=\"serverCultureInfo\" type=\"tns:serverCultureInfo\">"));
			assertTrue(result.contains("<xs:complexType name=\"serverCultureInfo\">"));
			assertTrue(result.contains("<cb:cobolElement byteLength=\"32\" cobolName=\"cultureCode\" levelNumber=\"5\" picture=\"X(32)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"));
		} catch (BuildException e) {
			fail(e.getMessage());
		}
	}

	public void testGenerationFromCollections() throws Exception {
		final String targetFile = "Container.xsd";
		JavaToXsdCobolTask task = new JavaToXsdCobolTask();
		task.setTargetDir(new File(TARGET_SCHEMA_DIR));
		task.setTargetXsdFileName(targetFile);
		task.addRootClass("com.legstar.xsdc.test.cases.collections.Container");
		task.setNamespace("http://legstar.com");
		try {
			task.execute();
			String result = getSource(TARGET_SCHEMA_DIR, targetFile);
			assertTrue(result.contains("targetNamespace=\"http://legstar.com\""));
			assertTrue(result.contains("<xs:complexType name=\"container\">"));
			assertTrue(result.contains("<xs:element maxOccurs=\"unbounded\" minOccurs=\"0\" name=\"itemsArray\" nillable=\"true\" type=\"tns:item\">"));
			assertTrue(result.contains("<cb:cobolType javaClassName=\"com.legstar.xsdc.test.cases.collections.Container\"/>"));
			assertTrue(result.contains("<cb:cobolType javaClassName=\"com.legstar.xsdc.test.cases.collections.Item\"/>"));
			assertTrue(result.contains("<cb:cobolElement cobolName=\"container\" levelNumber=\"1\" type=\"GROUP_ITEM\"/>"));
		} catch (BuildException e) {
			fail(e.getMessage());
		}
	}

	private String getSource(String srcLocation, String srcName) throws Exception {
        BufferedReader in = new BufferedReader(new FileReader(srcLocation + '/' + srcName));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            LOG.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        return resStr;
    }


}
