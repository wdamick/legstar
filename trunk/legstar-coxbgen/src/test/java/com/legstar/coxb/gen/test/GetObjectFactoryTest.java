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

public class GetObjectFactoryTest extends TestCase {
	
	/** Generated JAXB classes binaries. */
	private static final String JAXB_DIR = "../legstar-jaxbgen-cases/target/classes";
	
	/** Attempt to get factory object from the wrong location should fail. */
	public void testInvalidLocation() {
		try {
			Object of = CoxbBindingGenerator.getObjectFactory(
					"com.legstar.truc.coxb.alltypes",
					new File("gen-bin"));
			fail("Invalid location test failed " + of.getClass().getName());
		} catch (Exception e) {
			assertEquals("ClassNotFoundException com.legstar.truc.coxb.alltypes.ObjectFactory in gen-bin", e.getMessage());
		}
	}

	/** Location is correct but package does not exist. */
	public void testInvalidPackage() {
		try {
			CoxbBindingGenerator.getObjectFactory(
					"com.legstar.test.truc.ALLTYPES",
					new File(JAXB_DIR));
			fail("Invalid package test failed");
		} catch (Exception e) {
			assertEquals("ClassNotFoundException com.legstar.test.truc.ALLTYPES.ObjectFactory in ..\\legstar-jaxbgen-cases\\target\\classes", e.getMessage());
		}
	}

	/** Should succed since both package and location are correct*/
	public void testGetObjectFactory() {
		Object of = CoxbBindingGenerator.getObjectFactory(
				"com.legstar.test.coxb.alltypes",
				new File(JAXB_DIR));
		assertEquals("com.legstar.test.coxb.alltypes.ObjectFactory", of.getClass().getName());
	}

}
