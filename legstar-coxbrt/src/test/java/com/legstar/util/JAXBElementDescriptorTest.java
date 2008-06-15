package com.legstar.util;

import junit.framework.TestCase;

public class JAXBElementDescriptorTest extends TestCase {
	
	public void testXmlType() throws JAXBAnnotationException {
		JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
				"com.legstar.test.coxb.cultureinfo",
				"GetInfoType");
		assertEquals("JAXB package=com.legstar.test.coxb.cultureinfo, JAXB type=GetInfoType, XML element=getInfo, is XmlRootElement=false", elementDescriptor.toString());
	}

	public void testXmlRootElement() throws JAXBAnnotationException {
		JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
				"com.legstar.test.coxb.MSNSearch",
				"Search");
		assertEquals("JAXB package=com.legstar.test.coxb.MSNSearch, JAXB type=Search, XML element=Search, is XmlRootElement=true", elementDescriptor.toString());
	}
}
