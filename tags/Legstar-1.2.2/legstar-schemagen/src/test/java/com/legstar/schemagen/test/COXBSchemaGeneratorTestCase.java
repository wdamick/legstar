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
package com.legstar.schemagen.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.FileSet;

import junit.framework.TestCase;

import com.legstar.schemagen.COXBSchemaGenerator;

/**
 * Test the generator ANT task input handling.
 */
public class COXBSchemaGeneratorTestCase extends TestCase {

	/** COBOL source test cases directory. */
	private static final String COB_DIR = "./src/test/cobol/local";
	
	/** XML schema target directory. */
	private static final String XSD_DIR = "./target/schema/local";
	
	/** The current ANT project. */
	private Project mProject;
	
	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(COXBSchemaGeneratorTestCase.class);
	
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
			gen.setTargetDir(new File(XSD_DIR));
			gen.setNamespace("http://legstar.test/schemagen");
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
			gen.setTargetDir(new File(XSD_DIR));
			gen.setNamespace("http://legstar.test/schemagen");
			gen.setSourceCobolFilePath("cob.file");
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
			gen.setSourceCobolFilePath("cob.file");
			gen.setTargetDir(new File(XSD_DIR));
			gen.setTargetXsdFileName("xsd.file");
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
			gen.setTargetDir(new File(XSD_DIR));
			gen.setSourceCobolFilePath("cob.file");
			gen.setTargetXsdFileName("xsd.file");
			gen.setNamespace("^");
			gen.execute();
			fail("Check invalid namespace failed");
		} catch (Exception e) {
			assertEquals("java.net.URISyntaxException: Illegal character in path at index 0: ^", e.getMessage());
		}

	}

	/** Invalid target namespace. */
	public final void testOpaqueURI() {

		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setTargetDir(new File(XSD_DIR));
			gen.setSourceCobolFilePath("cob.file");
			gen.setTargetXsdFileName("xsd.file");
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
			gen.setTargetDir(new File(XSD_DIR));
			gen.setSourceCobolFilePath("cob.file");
			gen.setTargetXsdFileName("xsd.file");
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
			gen.setSourceCobolFilePath(COB_DIR + "/simplest.cob");
			gen.setTargetDir(new File(XSD_DIR));
			gen.setTargetXsdFileName("/xsd.file");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			assertEquals("com.sun.java.j2se.1.3", gen.getJaxbPackageName());
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
			gen.setTargetDir(new File(XSD_DIR));
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			assertEquals("com.sun.java.j2se.1.3", gen.getJaxbPackageName());
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
			gen.setSourceCobolFilePath(COB_DIR + "/simplest.cob");
			gen.addPath(pa);
			gen.setTargetDir(new File(XSD_DIR));
			gen.setTargetXsdFileName("/xsd.file");
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
			gen.setTargetDir(new File(XSD_DIR));
			gen.setTargetXsdFileName("/xsd.file");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			fail("Path and file failed " );
		} catch (Exception e) {
			assertEquals("You should not specify an XML schema file name when a path is provided", e.getMessage());
		}
	}
	
	@SuppressWarnings("deprecation")
	public void testOldSetXsdFile() {
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			Path pa = new Path(mProject);
			FileSet fs = new FileSet();
			fs.setDir(new File("./src/test/cobol"));
			fs.setExcludes("local/**");
			pa.addFileset(fs);
			gen.addPath(pa);
			gen.setXSDFile("./target/schema");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
		} catch (Exception e) {
			assertEquals("Line=5, Unrecognized symbol € in picture clause", e.getMessage());
		}
	}
	
	/** Test generation without a suffix.*/
	public final void testGenerationWithoutSuffix() {
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setSourceCobolFilePath(
					(new File("src/main/zos/cobol/LSFILEAE.cbl")).getAbsolutePath());
			gen.setTargetDir(new File(XSD_DIR));
			gen.setTargetXsdFileName("/xsd.file");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.execute();
			String result = getSource(XSD_DIR, "xsd.file");
			assertTrue(result.contains("<xs:element name=\"ComPersonal\" type=\"xsns:ComPersonal\">"));
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	/** Test generation with a suffix.*/
	public final void testGenerationWithSuffix() {
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		try {
			gen.setSourceCobolFilePath(
					(new File("src/main/zos/cobol/LSFILEAE.cbl")).getAbsolutePath());
			gen.setTargetDir(new File(XSD_DIR));
			gen.setTargetXsdFileName("/xsd.file");
			gen.setNamespace("http://java.sun.com/j2se/1.3/");
			gen.setJaxbTypeClassesSuffix("TypeSuffix");
			gen.execute();
			String result = getSource(XSD_DIR, "xsd.file");
			assertTrue(result.contains("<xs:element name=\"ComPersonal\" type=\"xsns:ComPersonalTypeSuffix\">"));
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	private String getSource(String srcLocation, String srcName) throws Exception {
        BufferedReader in = new BufferedReader(new FileReader(srcLocation + '/' + srcName));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            LOG.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        return resStr;
    }

}
