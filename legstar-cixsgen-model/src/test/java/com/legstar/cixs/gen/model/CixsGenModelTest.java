package com.legstar.cixs.gen.model;

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

}
