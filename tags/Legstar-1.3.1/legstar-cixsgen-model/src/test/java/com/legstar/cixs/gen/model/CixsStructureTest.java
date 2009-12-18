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
public class CixsStructureTest extends TestCase {
    
    /**
     * Serialize an input structure.
     */
    public void testSerialize() {
        CixsStructure structure = new CixsStructure();
        structure.setJaxbType("jaxbType");
        structure.setJaxbFieldName("jaxbFieldName");
        structure.setJaxbPropertyName("jaxbPropertyName");
        structure.setJaxbPackageName("jaxbPackageName");
        structure.setCoxbType("coxbType");
        structure.setCoxbPackageName("coxbPackageName");
        structure.setCobolRootDataItemName("cobolRootDataItemName");
        structure.setCicsContainer("cicsContainer");
        structure.setCustPackageName("custPackageName");
        assertEquals(
                "<input jaxbType=\"jaxbType\""
                + " jaxbPackageName=\"jaxbPackageName\""
                + " coxbType=\"coxbType\""
                + " coxbPackageName=\"coxbPackageName\""
                + " custPackageName=\"custPackageName\""
                + " cobolRootDataItemName=\"cobolRootDataItemName\""
                + " cicsContainer=\"cicsContainer\""
                + " jaxbPropertyName=\"jaxbPropertyName\""
                + " jaxbFieldName=\"jaxbFieldName\"/>",
                structure.serialize("input"));
    }

    /**
     * Deserialize an input structure.
     * @throws Exception if something goes wrong
     */
    public void testDeserialize() throws Exception {
        CixsStructure structure = new CixsStructure();
        DocumentBuilderFactory docFac =
            DocumentBuilderFactory.newInstance();
        DocumentBuilder db = docFac.newDocumentBuilder();
        Document doc = db.parse(new InputSource(new StringReader(
                "<input jaxbType=\"jaxbType\""
                + " jaxbPackageName=\"jaxbPackageName\""
                + " coxbType=\"coxbType\""
                + " coxbPackageName=\"coxbPackageName\""
                + " custPackageName=\"custPackageName\""
                + " cobolRootDataItemName=\"cobolRootDataItemName\""
                + " cicsContainer=\"cicsContainer\""
                + " jaxbPropertyName=\"jaxbPropertyName\""
                + " jaxbFieldName=\"jaxbFieldName\"/>")));
        structure.load(doc.getElementsByTagName("input").item(0));
        assertEquals("jaxbType", structure.getJaxbType());
        assertEquals("jaxbFieldName", structure.getJaxbFieldName());
        assertEquals("jaxbPropertyName", structure.getJaxbPropertyName());
        assertEquals("jaxbPackageName", structure.getJaxbPackageName());
        assertEquals("coxbType", structure.getCoxbType());
        assertEquals("coxbPackageName", structure.getCoxbPackageName());
        assertEquals("cobolRootDataItemName", structure.getCobolRootDataItemName());
        assertEquals("cicsContainer", structure.getCicsContainer());
        assertEquals("custPackageName", structure.getCustPackageName());
    }
}
