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
package com.legstar.coxb.gen;

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
