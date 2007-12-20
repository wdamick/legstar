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
package com.legstar.cobc.gen;

import java.io.File;
import java.io.IOException;

import org.apache.tools.ant.BuildException;

import junit.framework.TestCase;

public class CobolGeneratorTest extends TestCase {
	
	private static final boolean DEBUG_MODE = true;
	
	public void testCheckInput() {
		try {
			CobolGenerator gen = new CobolGenerator();
			gen.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("You must provide a JAXB type name", e.getMessage());
		}
		
		try {
			CobolGenerator gen = new CobolGenerator();
			gen.setJaxbTypeName("dfhcommarea");
			gen.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("You must provide a target directory", e.getMessage());
		}
	}
	
	public void testGetBinding() {
		try {
			CobolGenerator gen = new CobolGenerator();
			gen.setJaxbTypeName("dfhcommarea");
			gen.setTargetDir(new File("target"));
			gen.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("java.lang.ClassNotFoundException: ObjectFactory", e.getMessage());
		}

		try {
			CobolGenerator gen = new CobolGenerator();
			gen.setJaxbTypeName("dfhcommarea");
			gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
			gen.setTargetDir(new File("target"));
			gen.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("java.lang.ClassNotFoundException: com.legstar.test.coxb.lsfileae.dfhcommarea", e.getMessage());
		}
	}

	public void testGenerate() {
		/* Use default root name */
		try {
			CobolGenerator gen = new CobolGenerator();
			gen.setJaxbTypeName("DfhcommareaType");
			gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
			gen.setTargetDir(new File("target"));
			gen.execute();
			File outFile = new File("target/DfhcommareaType.cbl");
			String source = CobcUtil.getSource(outFile, DEBUG_MODE);
			assertTrue(source.contains("01 DfhcommareaType."));
			assertTrue(source.contains("02 COM-NUMBER PIC 9(6)."));
			assertTrue(source.contains("02 COM-PERSONAL."));
			assertTrue(source.contains("03 COM-NAME PIC X(20)."));
			assertTrue(source.contains("02 COM-DATE PIC X(8)."));
		} catch (BuildException e) {
			fail(e.getMessage());
		} catch (IOException e) {
			fail(e.getMessage());
		}

		/* Force a root name */
		try {
			CobolGenerator gen = new CobolGenerator();
			gen.setJaxbTypeName("DfhcommareaType");
			gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
			gen.setTargetDir(new File("target"));
			gen.setCobolRootDataItemName("COM-LSFILEAE");
			gen.execute();
			File outFile = new File("target/DfhcommareaType.cbl");
			String source = CobcUtil.getSource(outFile, DEBUG_MODE);
			assertTrue(source.contains("COM-LSFILEAE."));
			assertTrue(source.contains("02 COM-NUMBER PIC 9(6)."));
			assertTrue(source.contains("02 COM-PERSONAL."));
			assertTrue(source.contains("03 COM-NAME PIC X(20)."));
			assertTrue(source.contains("02 COM-DATE PIC X(8)."));
		} catch (BuildException e) {
			fail(e.getMessage());
		} catch (IOException e) {
			fail(e.getMessage());
		}

		/* Force a cobol file name */
		try {
			CobolGenerator gen = new CobolGenerator();
			gen.setJaxbTypeName("DfhcommareaType");
			gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
			gen.setTargetDir(new File("target"));
			gen.setCobolRootDataItemName("COM-LSFILEAE");
			gen.setTargetCobolFileName("lsfileae.cpy");
			gen.execute();
			File outFile = new File("target/lsfileae.cpy");
			String source = CobcUtil.getSource(outFile, DEBUG_MODE);
			assertTrue(source.contains("COM-LSFILEAE."));
			assertTrue(source.contains("02 COM-NUMBER PIC 9(6)."));
			assertTrue(source.contains("02 COM-PERSONAL."));
			assertTrue(source.contains("03 COM-NAME PIC X(20)."));
			assertTrue(source.contains("02 COM-DATE PIC X(8)."));
		} catch (BuildException e) {
			fail(e.getMessage());
		} catch (IOException e) {
			fail(e.getMessage());
		}
	}

	public void testGenerateCultureInfo() throws Exception {
		CobolGenerator gen = new CobolGenerator();
		gen.setJaxbTypeName("CultureInfoRequestType");
		gen.setJaxbPackageName("com.legstar.test.coxb.cultureinfo");
		gen.setTargetDir(new File("target"));
		gen.setCobolRootDataItemName("COM-REQUEST");
		gen.setTargetCobolFileName("cultureinfo-request.cpy");
		gen.execute();
		File outFile = new File("target/cultureinfo-request.cpy");
		String source = CobcUtil.getSource(outFile, DEBUG_MODE);
		assertTrue(source.contains("COM-REQUEST."));
		gen.setJaxbTypeName("CultureInfoReplyType");
		gen.setCobolRootDataItemName("COM-REPLY");
		gen.setTargetCobolFileName("cultureinfo-reply.cpy");
		gen.execute();
		File outFile2 = new File("target/cultureinfo-reply.cpy");
		String source2 = CobcUtil.getSource(outFile2, DEBUG_MODE);
		assertTrue(source2.contains("COM-REPLY."));
		
	}

}
