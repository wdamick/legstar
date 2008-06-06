package com.legstar.cixs.gen.model;

import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import junit.framework.TestCase;

public class CixsGenModelTest extends TestCase {
	
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
		assertEquals(serializedStructure(), result);
	}
	
	public void testLoadStructure() throws Exception {
    	DocumentBuilderFactory docBuilderFactory =
    		DocumentBuilderFactory.newInstance();
    	DocumentBuilder docBuilder;
		docBuilderFactory.setNamespaceAware(false);
		docBuilder = docBuilderFactory.newDocumentBuilder();
		Document doc = docBuilder.parse(new InputSource(new StringReader(serializedStructure())));
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
	
	private String serializedStructure() {
		StringBuilder sb = new StringBuilder();
		sb.append("<input jaxbType=\"jaxbType\" jaxbPackageName=\"jaxbPackageName\" coxbType=\"coxbType\" coxbPackageName=\"coxbPackageName\" cobolRootDataItemName=\"cobolRootDataItemName\" cicsContainer=\"cicsContainer\" jaxbPropertyName=\"jaxbPropertyName\" jaxbFieldName=\"jaxbFieldName\"/>");
		return sb.toString();
	}

}
