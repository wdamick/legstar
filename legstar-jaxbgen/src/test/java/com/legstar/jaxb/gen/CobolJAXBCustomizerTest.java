/*******************************************************************************
 * Copyright (c) 2011 LegSem.
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
        _cobolJAXBCustomizer.getXjbModel().setTypeNameSuffix("Type");
        _cobolJAXBCustomizer.customize(
new File(COB_XSD_DIR, "MSNSearch.xsd"),
                tempTargetXsdFile);
        String result = getSource(tempTargetXsdFile);
        assertTrue(result.contains("<jaxb:typeName suffix=\"Type\"/>"));
    }

    /**
     * Check that we add the JAXB namespace properly.
     */
    public void testInjectJAXBNamespace() {
        XmlSchema xsd = getXmlSchema(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                        + " targetNamespace=\"http://schemas.test.com\">"
                        + "</schema>");
        String prefix = _cobolJAXBCustomizer.injectJAXBNamespace(xsd);
        assertTrue(toString(xsd).contains(
                "xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""));
        assertEquals("jaxb", prefix);

        xsd = getXmlSchema(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                        + " xmlns:jxb=\"http://java.sun.com/xml/ns/jaxb\""
                        + " targetNamespace=\"http://schemas.test.com\">"
                        + "</schema>");
        prefix = _cobolJAXBCustomizer.injectJAXBNamespace(xsd);
        assertFalse(toString(xsd).contains(
                "xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""));
        assertEquals("jxb", prefix);
    }

    /**
     * Check that the extension attributes are handled correctly.
     */
    public void testInjectJAXBExtensionAttributes() {
        String result = null;

        XmlSchema xsd = getXmlSchema(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                        + " targetNamespace=\"http://schemas.test.com\">"
                        + "</schema>");
        String jaxbPrefix = _cobolJAXBCustomizer.injectJAXBNamespace(xsd);

        _cobolJAXBCustomizer.injectJAXBExtensionAttributes(xsd, jaxbPrefix);
        result = toString(xsd);
        assertTrue(result.contains(
                "jaxb:extensionBindingPrefixes=\"cb\""));
        assertTrue(result.contains(
                "jaxb:version=\"2.0\""));

        xsd = getXmlSchema(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                        + " targetNamespace=\"http://schemas.test.com\""
                        + " xmlns:ns0=\"http://java.sun.com/xml/ns/jaxb\""
                        + " ns0:extensionBindingPrefixes=\"ab\">"
                        + "</schema>");
        jaxbPrefix = _cobolJAXBCustomizer.injectJAXBNamespace(xsd);
        _cobolJAXBCustomizer.injectJAXBExtensionAttributes(xsd, jaxbPrefix);
        result = toString(xsd);
        assertTrue(result.contains(
                "ns0:extensionBindingPrefixes=\"ab cb\""));
        assertTrue(result.contains(
                "ns0:version=\"2.0\""));

        xsd = getXmlSchema(
                "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\""
                        + " targetNamespace=\"http://schemas.test.com\""
                        + " xmlns:ns0=\"http://java.sun.com/xml/ns/jaxb\""
                        + " ns0:version=\"2.0\""
                        + " ns0:extensionBindingPrefixes=\"cb\">"
                        + "</schema>");
        jaxbPrefix = _cobolJAXBCustomizer.injectJAXBNamespace(xsd);
        _cobolJAXBCustomizer.injectJAXBExtensionAttributes(xsd, jaxbPrefix);
        result = toString(xsd);
        assertTrue(result.contains(
                "ns0:extensionBindingPrefixes=\"cb\""));
        assertTrue(result.contains(
                "ns0:version=\"2.0\""));
    }

}
