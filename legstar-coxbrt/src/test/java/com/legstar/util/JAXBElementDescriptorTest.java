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
package com.legstar.util;

import junit.framework.TestCase;

/**
 * Test the JAXBElementDescriptor class which is useful to group all the JAXB
 * characteristics of an element.
 *
 */
public class JAXBElementDescriptorTest extends TestCase {

    /**
     * Test that metadata is extracted from the XmlType annotation.
     */
    public void testXmlType() {
        try {
            JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
                    "com.legstar.test.coxb.cultureinfo", "GetInfo");
            assertEquals("JAXB package=com.legstar.test.coxb.cultureinfo,"
                    + " JAXB type=GetInfo,"
                    + " XML namespace=http://cultureinfo.cases.test.xsdc.legstar.com/,"
                    + " XML element=getInfo,"
                    + " is XmlRootElement=false",
                    elementDescriptor.toString());
        } catch (JAXBAnnotationException e) {
            fail(e.getMessage());
        }
    }

    /**
     * This is a case where the element is a root element (it was root in the original
     * XSD).
     */
    public void testXmlRootElement() {
        try {
            JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
                    "com.legstar.test.coxb.MSNSearch", "Search");
            assertEquals("JAXB package=com.legstar.test.coxb.MSNSearch,"
                    + " JAXB type=Search,"
                    + " XML namespace=http://schemas.microsoft.com/MSNSearch/2005/09/fex,"
                    + " XML element=Search,"
                    + " is XmlRootElement=true",
                    elementDescriptor.toString());
        } catch (JAXBAnnotationException e) {
            fail(e.getMessage());
        }
    }
}
