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
package com.legstar.jaxb.gen;

import java.io.File;

import org.apache.ws.commons.schema.XmlSchema;

import com.legstar.jaxb.AbstractJaxbTester;

/**
 * Test the XJB generation.
 * 
 */
public class JaxbGenModelTest extends AbstractJaxbTester {

    /** The generation model. */
    private JaxbGenModel _model;

    /** The generated file. */
    private File _xjbFile;

    /**
     * Create model and file.
     * 
     * @throws Exception if output folder cannot be created
     */
    public void setUp() throws Exception {
        super.setUp();
        _model = new JaxbGenModel();
        _xjbFile = new File(GEN_XJB_DIR, "bindings.xjb");
    }

    /**
     * Test the generateIsSetMethod parameter.
     * 
     * @throws Exception if something goes wrong
     */
    public void testGenerateIsSetMethod() throws Exception {

        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("generateIsSetMethod=\"true\""));

        _model.setGenerateIsSetMethod(false);
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile)
                .contains("generateIsSetMethod=\"false\""));

    }

    /**
     * Test the serializableUid parameter.
     * 
     * @throws Exception if something goes wrong
     */
    public void testGenerateSerializableUid() throws Exception {

        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("serializable uid=\"1\""));

        _model.setSerializableUid(2567889120454L);
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains(
                "serializable uid=\"2567889120454\""));

    }

    /**
     * Test the prefix/suffix parameters.
     * 
     * @throws Exception if something goes wrong
     */
    public void testPrefixesAndSuffixes() throws Exception {

        _model.setXsdLocation("test.xsd");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains(
                "bindings schemaLocation=\"test.xsd\""));

        _model.setElementNamePrefix("A");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("prefix=\"A\""));

        _model.setElementNameSuffix("B");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("prefix=\"A\""));
        assertTrue(getSource(_xjbFile).contains("suffix=\"B\""));

        _model.setTypeNamePrefix("C");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("prefix=\"C\""));

        _model.setTypeNameSuffix("D");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("suffix=\"D\""));
    }

    /**
     * Test injecting default JAXB annotations in a schema.
     * 
     * @throws Exception if test fails
     */
    public void testInjectDefaultAnnotations() throws Exception {
        XmlSchema xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""
                + " targetNamespace=\"http://schemas.test.com\">" + "</schema>");
        _model.injectJaxbAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        String result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:globalBindings generateIsSetMethod=\"true\">"));
        assertTrue(result.contains("<jaxb:serializable uid=\"1\"/>"));
    }

    /**
     * Test injecting global JAXB annotations in a schema.
     * 
     * @throws Exception if test fails
     */
    public void testInjectGlobalAnnotations() throws Exception {
        XmlSchema xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""
                + " targetNamespace=\"http://schemas.test.com\">" + "</schema>");
        _model.setGenerateIsSetMethod(false);
        _model.setSerializableUid(1236895452412L);
        _model.injectJaxbAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        String result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:globalBindings generateIsSetMethod=\"false\">"));
        assertTrue(result
                .contains("<jaxb:serializable uid=\"1236895452412\"/>"));
    }

    /**
     * Test injecting schema JAXB annotations in a schema.
     * 
     * @throws Exception if test fails
     */
    public void testInjectSchemaAnnotations() throws Exception {
        XmlSchema xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""
                + " targetNamespace=\"http://schemas.test.com\">" + "</schema>");
        _model.setElementNamePrefix("ElementNamePrefix");
        _model.injectJaxbAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        String result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:elementName prefix=\"ElementNamePrefix\"/>"));

        _model.setElementNameSuffix("ElementNameSuffix");
        _model.injectJaxbAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:elementName prefix=\"ElementNamePrefix\" suffix=\"ElementNameSuffix\"/>"));

        _model.setTypeNamePrefix("TypeNamePrefix");
        _model.injectJaxbAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:typeName prefix=\"TypeNamePrefix\"/>"));

        _model.setTypeNameSuffix("TypeNameSuffix");
        _model.injectJaxbAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:typeName prefix=\"TypeNamePrefix\" suffix=\"TypeNameSuffix\"/>"));
    }

    /**
     * Test injecting global JAXB annotations in a schema that already contains
     * global annotations.
     * 
     * @throws Exception if test fails
     */
    public void testInjectGlobalAnnotationsAboveExisting() throws Exception {
        XmlSchema xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""
                + " targetNamespace=\"http://schemas.test.com\""
                + " jaxb:version=\"2.0\">"
                + "<annotation>"
                + "<appinfo>"
                + "<jaxb:globalBindings underscoreBinding=\"asCharInWord\""
                + " generateElementProperty=\"false\">"
                + "<jaxb:serializable />"
                + "</jaxb:globalBindings>"
                + "</appinfo>" + "</annotation>" + "</schema>");
        _model.setGenerateIsSetMethod(false);
        _model.setSerializableUid(1236895452412L);
        _model.injectJaxbAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        String result = toString(xsd);
        assertTrue(result.contains("<jaxb:globalBindings"));
        assertTrue(result.contains("generateIsSetMethod=\"false\""));
        assertTrue(result.contains("underscoreBinding=\"asCharInWord\""));
        assertTrue(result.contains("generateElementProperty=\"false\""));
        assertTrue(result
                .contains("<jaxb:serializable uid=\"1236895452412\"/>"));
    }
}
