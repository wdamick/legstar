package com.legstar.jaxb.gen;

import java.io.File;

import org.apache.ws.commons.schema.XmlSchema;

import com.legstar.jaxb.AbstractJaxbTester;

/**
 * Test the XJB generation.
 * 
 */
public class CobolJAXBXJBModelTest extends AbstractJaxbTester {

    /** The generation model. */
    private CobolJAXBXJBModel _model;

    /** The generated file. */
    private File _xjbFile;

    /**
     * Create model and file.
     * 
     * @throws Exception if output folder cannot be created
     */
    public void setUp() throws Exception {
        super.setUp();
        _model = new CobolJAXBXJBModel();
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
        assertTrue(getSource(_xjbFile).contains(
                "prefix=\"A\""));

        _model.setElementNameSuffix("B");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains(
                "prefix=\"A\""));
        assertTrue(getSource(_xjbFile).contains(
                "suffix=\"B\""));

        _model.setTypeNamePrefix("C");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains(
                "prefix=\"C\""));

        _model.setTypeNameSuffix("D");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains(
                "suffix=\"D\""));
    }

    /**
     * Test injecting default JAXB annotations in a schema.
     * 
     * @throws Exception if test fails
     */
    public void testInjectDefaultAnnotations() throws Exception {
        XmlSchema xsd = getXmlSchema(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                        + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""
                        + " targetNamespace=\"http://schemas.test.com\">"
                        + "</schema>");
        _model.injectAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        String result = toString(xsd);
        assertTrue(result.contains(
                "<jaxb:globalBindings generateIsSetMethod=\"true\">"));
        assertTrue(result.contains(
                "<jaxb:serializable uid=\"1\"/>"));
    }

    /**
     * Test injecting global JAXB annotations in a schema.
     * 
     * @throws Exception if test fails
     */
    public void testInjectGlobalAnnotations() throws Exception {
        XmlSchema xsd = getXmlSchema(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                        + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""
                        + " targetNamespace=\"http://schemas.test.com\">"
                        + "</schema>");
        _model.setGenerateIsSetMethod(false);
        _model.setSerializableUid(1236895452412L);
        _model.injectAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        String result = toString(xsd);
        assertTrue(result.contains(
                "<jaxb:globalBindings generateIsSetMethod=\"false\">"));
        assertTrue(result.contains(
                "<jaxb:serializable uid=\"1236895452412\"/>"));
    }

    /**
     * Test injecting schema JAXB annotations in a schema.
     * 
     * @throws Exception if test fails
     */
    public void testInjectSchemaAnnotations() throws Exception {
        XmlSchema xsd = getXmlSchema(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                        + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""
                        + " targetNamespace=\"http://schemas.test.com\">"
                        + "</schema>");
        _model.setElementNamePrefix("ElementNamePrefix");
        _model.injectAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        String result = toString(xsd);
        assertTrue(result.contains(
                "<jaxb:elementName prefix=\"ElementNamePrefix\"/>"));

        _model.setElementNameSuffix("ElementNameSuffix");
        _model.injectAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        result = toString(xsd);
        assertTrue(result
                .contains(
                "<jaxb:elementName prefix=\"ElementNamePrefix\" suffix=\"ElementNameSuffix\"/>"));

        _model.setTypeNamePrefix("TypeNamePrefix");
        _model.injectAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        result = toString(xsd);
        assertTrue(result.contains(
                "<jaxb:typeName prefix=\"TypeNamePrefix\"/>"));

        _model.setTypeNameSuffix("TypeNameSuffix");
        _model.injectAnnotations(xsd, "http://java.sun.com/xml/ns/jaxb",
                "jaxb", newDocument());
        result = toString(xsd);
        assertTrue(result
                .contains(
                "<jaxb:typeName prefix=\"TypeNamePrefix\" suffix=\"TypeNameSuffix\"/>"));
    }
}
