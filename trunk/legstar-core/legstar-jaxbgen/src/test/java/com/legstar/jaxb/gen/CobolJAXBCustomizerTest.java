/*******************************************************************************
 * Copyright (c) 2015 LegSem.
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

import com.legstar.jaxb.AbstractJaxbGenTest;

/**
 * Test cases for CobolJAXBCustomizer.
 * 
 */
public class CobolJAXBCustomizerTest extends AbstractJaxbGenTest {

    /** An instance of the customizer under test. */
    private CobolJAXBCustomizer _cobolJAXBCustomizer;

    /** {@inheritDoc} */
    @Override
    public void setUp() throws Exception {
        super.setUp();
        _cobolJAXBCustomizer = new CobolJAXBCustomizer(new JaxbGenModel());
    }

    /**
     * Test MSNSearch.
     * 
     * @throws Exception if test fails
     */
    public void testMSNSearch() throws Exception {

        File tempTargetXsdFile = File.createTempFile("jaxb-schema", "tmp");
        tempTargetXsdFile.deleteOnExit();
        _cobolJAXBCustomizer.getJaxbGenModel().setTypeNameSuffix("Type");
        _cobolJAXBCustomizer.customize(new File(COB_XSD_DIR, "MSNSearch.xsd"),
                tempTargetXsdFile);
        String result = getSource(tempTargetXsdFile);
        assertTrue(result.contains("<jaxb:typeName suffix=\"Type\"/>"));
    }

    /**
     * Check that we add the JAXB namespace properly.
     */
    public void testInjectJAXBNamespace() {
        XmlSchema xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " targetNamespace=\"http://schemas.test.com\">" + "</schema>");
        String prefix = _cobolJAXBCustomizer.injectJaxbNamespace(xsd);
        assertTrue(toString(xsd).contains(
                "xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""));
        assertEquals("jaxb", prefix);

        xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:jxb=\"http://java.sun.com/xml/ns/jaxb\""
                + " targetNamespace=\"http://schemas.test.com\">" + "</schema>");
        prefix = _cobolJAXBCustomizer.injectJaxbNamespace(xsd);
        assertFalse(toString(xsd).contains(
                "xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""));
        assertEquals("jxb", prefix);
    }

    /**
     * Check that the extension attributes are handled correctly.
     */
    public void testInjectJAXBExtensionAttributes() {
        String result = null;

        XmlSchema xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " targetNamespace=\"http://schemas.test.com\">" + "</schema>");
        String jaxbPrefix = _cobolJAXBCustomizer.injectJaxbNamespace(xsd);

        _cobolJAXBCustomizer.injectJaxbExtensionAttributes(xsd, jaxbPrefix);
        result = toString(xsd);
        assertTrue(result.contains("jaxb:extensionBindingPrefixes=\"cb\""));
        assertTrue(result.contains("jaxb:version=\"2.0\""));

        xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " targetNamespace=\"http://schemas.test.com\""
                + " xmlns:ns0=\"http://java.sun.com/xml/ns/jaxb\""
                + " ns0:extensionBindingPrefixes=\"ab\">" + "</schema>");
        jaxbPrefix = _cobolJAXBCustomizer.injectJaxbNamespace(xsd);
        _cobolJAXBCustomizer.injectJaxbExtensionAttributes(xsd, jaxbPrefix);
        result = toString(xsd);
        assertTrue(result.contains("ns0:extensionBindingPrefixes=\"ab cb\""));
        assertTrue(result.contains("ns0:version=\"2.0\""));

        xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " targetNamespace=\"http://schemas.test.com\""
                + " xmlns:ns0=\"http://java.sun.com/xml/ns/jaxb\""
                + " ns0:version=\"2.0\""
                + " ns0:extensionBindingPrefixes=\"cb\">" + "</schema>");
        jaxbPrefix = _cobolJAXBCustomizer.injectJaxbNamespace(xsd);
        _cobolJAXBCustomizer.injectJaxbExtensionAttributes(xsd, jaxbPrefix);
        result = toString(xsd);
        assertTrue(result.contains("ns0:extensionBindingPrefixes=\"cb\""));
        assertTrue(result.contains("ns0:version=\"2.0\""));
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
        _cobolJAXBCustomizer.injectJaxbSchemaAnnotations(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");
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
        _cobolJAXBCustomizer.getJaxbGenModel().setGenerateIsSetMethod(false);
        _cobolJAXBCustomizer.getJaxbGenModel().setSerializableUid(
                1236895452412L);
        _cobolJAXBCustomizer.injectJaxbSchemaAnnotations(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");
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
        _cobolJAXBCustomizer.getJaxbGenModel().setElementNamePrefix(
                "ElementNamePrefix");
        _cobolJAXBCustomizer.injectJaxbSchemaAnnotations(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");
        String result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:elementName prefix=\"ElementNamePrefix\"/>"));

        _cobolJAXBCustomizer.getJaxbGenModel().setElementNameSuffix(
                "ElementNameSuffix");
        _cobolJAXBCustomizer.injectJaxbSchemaAnnotations(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");
        result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:elementName prefix=\"ElementNamePrefix\" suffix=\"ElementNameSuffix\"/>"));

        _cobolJAXBCustomizer.getJaxbGenModel().setTypeNamePrefix(
                "TypeNamePrefix");
        _cobolJAXBCustomizer.injectJaxbSchemaAnnotations(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");
        result = toString(xsd);
        assertTrue(result
                .contains("<jaxb:typeName prefix=\"TypeNamePrefix\"/>"));

        _cobolJAXBCustomizer.getJaxbGenModel().setTypeNameSuffix(
                "TypeNameSuffix");
        _cobolJAXBCustomizer.injectJaxbSchemaAnnotations(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");
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
        _cobolJAXBCustomizer.getJaxbGenModel().setGenerateIsSetMethod(false);
        _cobolJAXBCustomizer.getJaxbGenModel().setSerializableUid(
                1236895452412L);
        _cobolJAXBCustomizer.injectJaxbSchemaAnnotations(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");
        String result = toString(xsd);
        assertTrue(result.contains("<jaxb:globalBindings"));
        assertTrue(result.contains("generateIsSetMethod=\"false\""));
        assertTrue(result.contains("underscoreBinding=\"asCharInWord\""));
        assertTrue(result.contains("generateElementProperty=\"false\""));
        assertTrue(result
                .contains("<jaxb:serializable uid=\"1236895452412\"/>"));
    }

    /**
     * Test JAXB customization in ECI compatible mode.
     * 
     * @throws Exception if test fails
     */
    public void testCustomizationWithEciCompatible() throws Exception {
        XmlSchema xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + " targetNamespace=\"http://schemas.test.com\">" + "</schema>");
        _cobolJAXBCustomizer.getJaxbGenModel().setEciCompatible(true);
        _cobolJAXBCustomizer.injectJaxbSchemaAnnotations(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");
        String result = toString(xsd);
        assertTrue(result.contains("<jaxb:globalBindings"));
        assertTrue(result.contains("collectionType=\"indexed\""));
    }

    /**
     * Test injection of JAXB annotation jaxb:typesafeEnumClass on anonymous
     * sumpleType with enumeration restriction.
     * 
     * @throws Exception if test fails
     */
    public void testInjectTypesafeEnumClass() throws Exception {
        XmlSchema xsd = getXmlSchema("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                + "    xmlns:tns=\"http://coxb.test.legstar.com/level88\""
                + "    targetNamespace=\"http://coxb.test.legstar.com/level88\">"
                + "    <complexType name=\"Dfhcommarea\">"
                + "        <sequence>"
                + "            <element name=\"myvalue\">"
                + "                <simpleType>"
                + "                    <restriction base=\"string\">"
                + "                        <maxLength value=\"5\"/>"
                + "                        <enumeration value=\"true\"/>"
                + "                        <enumeration value=\"false\"/>"
                + "                    </restriction>"
                + "                </simpleType>"
                + "            </element>"
                + "        </sequence>"
                + "    </complexType>"
                + "    <element name=\"dfhcommarea\" type=\"tns:Dfhcommarea\"/>"
                + "</schema>");

        _cobolJAXBCustomizer.injectJaxbEnumerationsAnnotation(xsd,
                "http://java.sun.com/xml/ns/jaxb", "jaxb");

        String result = toString(xsd);
        assertTrue(result.contains("<jaxb:typesafeEnumClass name=\"Myvalue\""
                + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\"/>"));
        assertTrue(result
                .contains("<jaxb:typesafeEnumMember name=\"VALUE_TRUE\""
                        + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\"/>"));
        assertTrue(result
                .contains("<jaxb:typesafeEnumMember name=\"VALUE_FALSE\""
                        + " xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\"/>"));
    }

}
