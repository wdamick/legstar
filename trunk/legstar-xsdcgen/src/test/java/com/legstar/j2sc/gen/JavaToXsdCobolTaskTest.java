/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.j2sc.gen;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.tools.ant.BuildException;

import com.legstar.coxb.util.ClassUtil;

/**
 * Test Java To Xsd Generator.
 * 
 */
public class JavaToXsdCobolTaskTest extends AbstractTest {

    /**
     * Check input validations.
     * 
     * @throws Exception if test fails
     */
    public void testInputValidations() throws Exception {
        getJavaToXsdCobolTask().setTargetDir(null);
        try {
            getJavaToXsdCobolTask().execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must provide a target directory", e.getMessage());
        }
        getJavaToXsdCobolTask().setTargetDir(new File("bidule"));
        try {
            getJavaToXsdCobolTask().execute();
            fail();
        } catch (BuildException e) {
            assertEquals("Directory bidule does not exist", e.getMessage());
        }
        getJavaToXsdCobolTask().setTargetDir(GEN_DIR);
        try {
            getJavaToXsdCobolTask().execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must provide a target xsd file name", e
                    .getMessage());
        }
        getJavaToXsdCobolTask().setTargetXsdFileName("JVMQuery.xsd");
        try {
            getJavaToXsdCobolTask().execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must specify an output XML schema namespace", e
                    .getMessage());
        }
        getJavaToXsdCobolTask().setNamespace("http://legstar.test");
        try {
            getJavaToXsdCobolTask().execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must provide at least one class name", e
                    .getMessage());
        }
        getJavaToXsdCobolTask().addRootClass("JVMQuery");
        try {
            getJavaToXsdCobolTask().execute();
            fail();
        } catch (BuildException e) {
            assertEquals("java.lang.ClassNotFoundException: JVMQuery", e
                    .getMessage());
        }
    }

    /**
     * Generate a schema from a POJO.
     * 
     * @throws Exception if generation fails
     */
    public void testGenerationFromPojo() throws Exception {
        final String targetFile = "JVMQueryReply.xsd";
        getJavaToXsdCobolTask().setTargetXsdFileName(targetFile);
        getJavaToXsdCobolTask().addRootClass(
                "com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest");
        getJavaToXsdCobolTask().addRootClass(
                "com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply");
        getJavaToXsdCobolTask().setNamespace("http://legstar.com");
        try {
            getJavaToXsdCobolTask().execute();
            String result = getSource(GEN_DIR, targetFile);
            assertTrue(result
                    .contains("targetNamespace=\"http://legstar.com\""));
            assertTrue(result.contains("<xs:element"
                    + " name=\"jvmQueryRequest\""
                    + " type=\"jvmQueryRequest\">"));
            assertTrue(result.contains("cb:cobolElement"
                    + " cobolName=\"envVarNames\""
                    + " levelNumber=\"3\""
                    + " maxOccurs=\"10\""
                    + " minOccurs=\"0\""
                    + " picture=\"X(32)\""
                    + " type=\"ALPHANUMERIC_ITEM\""
                    + " usage=\"DISPLAY\"/>"));
            assertTrue(result.contains("<xs:element"
                    + " name=\"jvmQueryReply\""
                    + " type=\"jvmQueryReply\">"));
            assertTrue(result.contains("<cb:cobolElement"
                    + " cobolName=\"country\""
                    + " levelNumber=\"3\""
                    + " picture=\"X(32)\""
                    + " type=\"ALPHANUMERIC_ITEM\""
                    + " usage=\"DISPLAY\"/>"));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Check that a complex type map is successfully created.
     * 
     * @throws Exception if generation fails
     */
    public void testComplexTypeToJavaMapping() throws Exception {
        final String targetFile = "JVMQueryReply.xsd";
        getJavaToXsdCobolTask().setNamespace("http://legstar.com");
        Class < ? >[] classes = { ClassUtil
                .loadClass("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply") };
        Map < String, String > complexTypeToJavaClassMap = new HashMap < String, String >();

        try {
            getJavaToXsdCobolTask().generateSchema(classes,
                    new File(GEN_DIR + targetFile), complexTypeToJavaClassMap);
            String result = getSource(GEN_DIR, targetFile);
            assertTrue(result
                    .contains("targetNamespace=\"http://legstar.com\""));
            assertTrue(result
                    .contains("<xs:element name=\"jvmQueryReply\" type=\"jvmQueryReply\">"));
            assertTrue(complexTypeToJavaClassMap.size() == 1);
            assertEquals("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply",
                    complexTypeToJavaClassMap.get("jvmQueryReply"));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test generation from a COBOL annotated class.
     * 
     * @throws Exception if generation fails
     */
    public void testGenerationFromAnnotated() throws Exception {
        final String targetFile = "CultureInfoRequest.xsd";
        getJavaToXsdCobolTask().setTargetXsdFileName(targetFile);
        getJavaToXsdCobolTask().addRootClass(
                "com.legstar.xsdc.test.cases.cultureinfo.CultureInfoRequest");
        getJavaToXsdCobolTask().setNamespace("http://legstar.com");
        try {
            getJavaToXsdCobolTask().execute();
            String result = getSource(GEN_DIR, targetFile);
            assertTrue(result
                    .contains("targetNamespace=\"http://legstar.com\""));
            assertTrue(result.contains("<xs:element"
                    + " name=\"cultureInfoParameters\""
                    + " type=\"cultureInfoParameters\">"));
            assertTrue(result
                    .contains("<cb:cobolComplexType"
                            + " javaClassName=\"com.legstar.xsdc.test.cases.cultureinfo.CultureInfoRequest\"/>"));
            assertTrue(result.contains("<cb:cobolElement"
                    + " cobolName=\"cultureInfoParameters\""
                    + " levelNumber=\"1\""
                    + " type=\"GROUP_ITEM\"/>"));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test generation from a class containing another.
     * 
     * @throws Exception if generation fails
     */
    public void testGenerationFromAggregate() throws Exception {
        final String targetFile = "CultureInfoReply.xsd";
        getJavaToXsdCobolTask().setTargetXsdFileName(targetFile);
        getJavaToXsdCobolTask().addRootClass(
                "com.legstar.xsdc.test.cases.cultureinfo.CultureInfoReply");
        getJavaToXsdCobolTask().setNamespace("http://legstar.com");
        try {
            getJavaToXsdCobolTask().execute();
            String result = getSource(GEN_DIR, targetFile);
            assertTrue(result
                    .contains("targetNamespace=\"http://legstar.com\""));
            assertTrue(result
                    .contains("<xs:complexType name=\"cultureInfoReply\">"));
            assertTrue(result.contains("<cb:cobolElement"
                    + " cobolName=\"currencySymbol\""
                    + " levelNumber=\"3\""
                    + " picture=\"X(32)\""
                    + " type=\"ALPHANUMERIC_ITEM\""
                    + " usage=\"DISPLAY\"/>"));
            assertTrue(result.contains("<xs:element"
                    + " minOccurs=\"0\""
                    + " name=\"serverCultureInfo\""
                    + " type=\"tns:serverCultureInfo\">"));
            assertTrue(result
                    .contains("<xs:complexType name=\"serverCultureInfo\">"));
            assertTrue(result.contains("<cb:cobolElement"
                    + " cobolName=\"cultureCode\""
                    + " levelNumber=\"5\""
                    + " picture=\"X(32)\""
                    + " type=\"ALPHANUMERIC_ITEM\""
                    + " usage=\"DISPLAY\"/>"));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test generation from a COBOL annotated class with collections.
     * 
     * @throws Exception if generation fails
     */
    public void testGenerationFromCollections() throws Exception {
        final String targetFile = "Container.xsd";
        getJavaToXsdCobolTask().setTargetXsdFileName(targetFile);
        getJavaToXsdCobolTask().addRootClass(
                "com.legstar.xsdc.test.cases.collections.Container");
        getJavaToXsdCobolTask().setNamespace("http://legstar.com");
        try {
            getJavaToXsdCobolTask().execute();
            String result = getSource(GEN_DIR, targetFile);
            assertTrue(result
                    .contains("targetNamespace=\"http://legstar.com\""));
            assertTrue(result.contains("<xs:complexType name=\"container\">"));
            assertTrue(result.contains("<xs:element"
                    + " maxOccurs=\"unbounded\""
                    + " minOccurs=\"0\""
                    + " name=\"itemsArray\""
                    + " nillable=\"true\""
                    + " type=\"tns:item\">"));
            assertTrue(result
                    .contains("<cb:cobolComplexType"
                            + " javaClassName=\"com.legstar.xsdc.test.cases.collections.Container\"/>"));
            assertTrue(result
                    .contains("<cb:cobolComplexType"
                            + " javaClassName=\"com.legstar.xsdc.test.cases.collections.Item\"/>"));
            assertTrue(result.contains("<cb:cobolElement"
                    + " cobolName=\"container\""
                    + " levelNumber=\"1\""
                    + " type=\"GROUP_ITEM\"/>"));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

}
