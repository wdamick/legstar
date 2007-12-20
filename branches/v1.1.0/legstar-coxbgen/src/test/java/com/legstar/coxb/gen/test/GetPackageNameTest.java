/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.coxb.gen.test;

import java.io.File;

import junit.framework.TestCase;
import com.legstar.coxb.gen.CoxbBindingGenerator;

public class GetPackageNameTest extends TestCase {
	
	public void testInvalidFile() {
		
		try {
			CoxbBindingGenerator.getPackageName(new File("toto"));
			fail("invalid file test failed");
		} catch (Exception e) {
			assertEquals("IOException", e.getMessage().substring(0, 11));
		}
		
	}

	public void testNotXML() {
		
		try {
			CoxbBindingGenerator.getPackageName(new File("src/test/resources/notxml.xml"));
			fail("Not XML test failed");
		} catch (Exception e) {
			assertEquals("SAXException Content is not allowed in prolog.", e.getMessage());
		}
		
	}

	public void testNoJAXBAnnotations() {
		
		try {
			CoxbBindingGenerator.getPackageName(new File("src/test/resources/XSDWithoutAnnotations.xsd"));
			fail("Not JAXB annotations test failed");
		} catch (Exception e) {
			assertEquals("No JAXB annotations in XML schema file", e.getMessage());
		}
		
	}

	public void testWithJAXBAnnotations() {
		
		try {
			String pkg = CoxbBindingGenerator.getPackageName(new File("src/test/resources/ALLTYPES.xsd"));
			assertEquals("com.legstar.test.coxb.alltypes", pkg);
		} catch (Exception e) {
			fail("No annotations found " + e.getMessage());
		}
		
	}
}
