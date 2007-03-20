/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.schemagen.test;

import java.io.File;

import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.FileSet;

import junit.framework.TestCase;
import com.legstar.schemagen.COXBSchemaGenerator;

/**
 * Test the generator ANT task input handling.
 */
public class CheckInputTestCase extends TestCase {

	/** COBOL source test cases directory. */
	private static final String COB_DIR = "./src/test/cobol/local";
	
	/** XML schema target directory. */
	private static final String XSD_DIR = "./target/schema/local";
	
	/** The current ANT project. */
	private Project mProject;
	
	
	/** Make sure we have an output folder. */
	protected void setUp() throws Exception {
		java.io.File td = new java.io.File(XSD_DIR);
		td.mkdirs();
		mProject = new Project();
	}
	
	/** Cobol file not provided. */
	public final void testNoCobolFile() {

		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.execute();
			fail("Check cobol file failed");
		} catch (Exception e) {
			assertEquals("You must specify a cobol file name or a path",
					e.getMessage());
		}

	}

	/** XML schema file not provided. */
	public final void testNoXSDFile() {

		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setCobolFile("cob.file");
			gen.execute();
			fail("Check xsd file failed");
		} catch (Exception e) {
			assertEquals("You must specify an output XML schema file name",
					e.getMessage());
		}

	}

	/** Target namespace not provided. */
	public final void testNoNamespace() {

		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setCobolFile("cob.file");
			gen.setXSDFile("xsd.file");
			gen.execute();
			fail("Check no namespace failed");
		} catch (Exception e) {
			assertEquals("You must specify an output XML schema namespace",
					e.getMessage());
		}

	}

	/** Invalid target namespace. */
	public final void testInvalidURI() {

		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setCobolFile("cob.file");
			gen.setXSDFile("xsd.file");
			gen.setNamespace("^");
			gen.execute();
			fail("Check invalid namespace failed");
		} catch (Exception e) {
			assertEquals("The namespace ^ is invalid", e.getMessage());
		}

	}

	/** Invalid target namespace. */
	public final void testOpaqueURI() {

		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setCobolFile("cob.file");
			gen.setXSDFile("xsd.file");
			gen.setNamespace("mailto:java-net@java.sun.com");
			gen.execute();
			fail("Check opaque namespace failed");
		} catch (Exception e) {
			assertEquals(
					"Namespace mailto:java-net@java.sun.com is not a"
					 + " hierarchical URI", e.getMessage());
		}

	}

	/** Invalid cobol file. */
	public final void testInvalidCobolFile() {
		
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setCobolFile("cob.file");
			gen.setXSDFile("xsd.file");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			fail("Invalid cobol file test failed");
		} catch (Exception e) {
			assertEquals("Invalid input file cob.file", e.getMessage());
		}
		
	}
	/** Package name derived from namespace. */
	public final void testDerivedPackageName() {
		
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setCobolFile(COB_DIR + "/simplest.cob");
			gen.setXSDFile(XSD_DIR + "/xsd.file");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			assertEquals("com.sun.java.j2se.1.3", gen.getPackage());
		} catch (Exception e) {
			fail("Check derived package name failed " + e.getMessage());
		}
	}
	
	/** A path should be accepted as an alternative to a single cobol file.*/
	public final void testPathAccepted() {
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			Path pa = new Path(mProject);
			FileSet fs = new FileSet();
			fs.setDir(new File(COB_DIR));
			fs.setExcludes("**/currencySign.cob");
			pa.addFileset(fs);
			gen.addPath(pa);
			gen.setXSDFile(XSD_DIR + "/");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			assertEquals("com.sun.java.j2se.1.3", gen.getPackage());
		} catch (Exception e) {
			fail("Path instead of file failed " + e.getMessage());
		}
	}

	/** User should not specify both a file and a path.*/
	public final void testPathAnfFile() {
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			Path pa = new Path(mProject);
			FileSet fs = new FileSet();
			fs.setDir(new File(COB_DIR));
			pa.addFileset(fs);
			gen.setCobolFile(COB_DIR + "/simplest.cob");
			gen.addPath(pa);
			gen.setXSDFile(XSD_DIR + "/xsd.file");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			fail("Path and file failed " );
		} catch (Exception e) {
			assertEquals("You must specify either a cobol file name or a path (but not both)", e.getMessage());
		}
	}

	/** User should not specify a folder for XSDs when path option is used.*/
	public final void testFolder4XSD() {
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			Path pa = new Path(mProject);
			FileSet fs = new FileSet();
			fs.setDir(new File(COB_DIR));
			pa.addFileset(fs);
			gen.addPath(pa);
			gen.setXSDFile(XSD_DIR + "/xsd.file");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			fail("Path and file failed " );
		} catch (Exception e) {
			assertEquals("XSD file should be a folder name (end with '/') when path option is used", e.getMessage());
		}
	}
}
