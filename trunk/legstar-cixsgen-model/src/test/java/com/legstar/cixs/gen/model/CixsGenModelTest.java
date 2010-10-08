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
package com.legstar.cixs.gen.model;

import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import junit.framework.TestCase;

/**
 * Test the CixsStructure class.
 *
 */
public class CixsGenModelTest extends TestCase {
    
    /** A sample serialized structure.*/
    public static final String SERIALIZED_STRUCTURE = "<input jaxbType=\"jaxbType\""
        + " jaxbPackageName=\"jaxbPackageName\""
        + " coxbType=\"coxbType\""
        + " coxbPackageName=\"coxbPackageName\""
        + " cobolRootDataItemName=\"cobolRootDataItemName\""
        + " cicsContainer=\"cicsContainer\""
        + " jaxbPropertyName=\"jaxbPropertyName\""
        + " jaxbFieldName=\"jaxbFieldName\"/>";

    /**
     * Check members.
     */
    public void testStructure() {
        CixsStructure structure = new CixsStructure();
        assertTrue(null == structure.getJaxbPropertyName());
        structure.setJaxbFieldName("jaxbFieldName");
        assertEquals("JaxbFieldName", structure.getJaxbPropertyName());
        assertEquals("jaxbFieldName", structure.getJaxbFieldName());
        structure.setJaxbFieldName(null);
        assertTrue(null == structure.getJaxbFieldName());
        structure.setJaxbPropertyName("JaxbPropertyName");
        assertEquals("jaxbPropertyName", structure.getJaxbFieldName());
        assertEquals("JaxbPropertyName", structure.getJaxbPropertyName());
        structure.setJaxbPropertyName(null);
        assertTrue(null == structure.getJaxbPropertyName());
        structure.setJaxbType("JaxbType");
        assertEquals("jaxb", structure.getJaxbFieldName());
        assertEquals("Jaxb", structure.getJaxbPropertyName());
    }

    /**
     * Serialize a CixsStructure.
     */
    public void testSerializeStructure() {
        CixsStructure structure = new CixsStructure();
        structure.setCicsContainer("cicsContainer");
        structure.setCobolRootDataItemName("cobolRootDataItemName");
        structure.setCoxbPackageName("coxbPackageName");
        structure.setCoxbType("coxbType");
        structure.setJaxbFieldName("jaxbFieldName");
        structure.setJaxbPackageName("jaxbPackageName");
        structure.setJaxbPropertyName("jaxbPropertyName");
        structure.setJaxbType("jaxbType");
        String result = structure.serialize("input");
        System.out.println(result);
        assertEquals(SERIALIZED_STRUCTURE, result);
    }

    /**
     * Deserialize a CixsStructure.
     * @throws Exception if deserializing fails
     */
    public void testLoadStructure() throws Exception {
        DocumentBuilderFactory docBuilderFactory =
            DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder;
        docBuilderFactory.setNamespaceAware(false);
        docBuilder = docBuilderFactory.newDocumentBuilder();
        Document doc = docBuilder.parse(new InputSource(new StringReader(SERIALIZED_STRUCTURE)));
        CixsStructure structure = new CixsStructure();
        structure.load(doc.getFirstChild());
        assertEquals("cicsContainer", structure.getCicsContainer());
        assertEquals("cobolRootDataItemName", structure.getCobolRootDataItemName());
        assertEquals("coxbPackageName", structure.getCoxbPackageName());
        assertEquals("coxbType", structure.getCoxbType());
        assertEquals("jaxbFieldName", structure.getJaxbFieldName());
        assertEquals("jaxbPackageName", structure.getJaxbPackageName());
        assertEquals("jaxbPropertyName", structure.getJaxbPropertyName());
        assertEquals("jaxbType", structure.getJaxbType());
    }

}
