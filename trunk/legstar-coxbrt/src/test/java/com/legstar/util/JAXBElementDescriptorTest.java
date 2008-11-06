/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.util;

import junit.framework.TestCase;

public class JAXBElementDescriptorTest extends TestCase {
	
	public void testXmlType() throws JAXBAnnotationException {
		JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
				"com.legstar.test.coxb.cultureinfo",
				"GetInfo");
		assertEquals("JAXB package=com.legstar.test.coxb.cultureinfo, JAXB type=GetInfo, XML element=getInfo, is XmlRootElement=false", elementDescriptor.toString());
	}

	public void testXmlRootElement() throws JAXBAnnotationException {
		JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
				"com.legstar.test.coxb.MSNSearch",
				"Search");
		assertEquals("JAXB package=com.legstar.test.coxb.MSNSearch, JAXB type=Search, XML element=Search, is XmlRootElement=true", elementDescriptor.toString());
	}
}
