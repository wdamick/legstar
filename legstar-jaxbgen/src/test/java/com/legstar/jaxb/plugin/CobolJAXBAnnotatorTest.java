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
package com.legstar.jaxb.plugin;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Commandline;

import com.sun.tools.xjc.XJC2Task;

import junit.framework.TestCase;

public class CobolJAXBAnnotatorTest extends TestCase {
	
	private static final boolean DEBUG_MODE = false;
	private static final String SCHEMA_DIR = "../legstar-schemagen/src/test/schema";
	private static final String GEN_SRC_DIR = "src/test/gen/java";
	private static final String GEN_SRC_SUBDIR = "com/legstar/test/coxb";
	

	public void testSimpleAnnotation() {
		genSource("alltypes");
		String srce = getSource("alltypes", "DfhCommareaType" );
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-STRING\", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, byteLength = 4, picture = \"X(4)\", usage = \"DISPLAY\", srceLine = 24)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-BINARY\", type = CobolType.OCTET_STREAM_ITEM, levelNumber = 5, byteLength = 4, picture = \"X(4)\", usage = \"DISPLAY\", srceLine = 25)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-SHORT\", type = CobolType.BINARY_ITEM, levelNumber = 5, byteLength = 2, isSigned = true, totalDigits = 4, picture = \"S9(4)\", usage = \"BINARY\", srceLine = 26)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-USHORT\", type = CobolType.BINARY_ITEM, levelNumber = 5, byteLength = 2, isSigned = false, totalDigits = 4, picture = \"9(4)\", usage = \"BINARY\", srceLine = 27)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-INT\", type = CobolType.BINARY_ITEM, levelNumber = 5, byteLength = 4, isSigned = true, totalDigits = 9, picture = \"S9(9)\", usage = \"BINARY\", srceLine = 28)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-UINT\", type = CobolType.BINARY_ITEM, levelNumber = 5, byteLength = 4, isSigned = false, totalDigits = 9, picture = \"9(9)\", usage = \"BINARY\", srceLine = 29)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-LONG\", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, byteLength = 10, isSigned = true, totalDigits = 18, picture = \"S9(18)\", usage = \"PACKED-DECIMAL\", srceLine = 30)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-ULONG\", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, byteLength = 10, isSigned = false, totalDigits = 18, picture = \"9(18)\", usage = \"PACKED-DECIMAL\", srceLine = 31)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-XLONG\", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, byteLength = 16, isSigned = true, totalDigits = 31, picture = \"S9(31)\", usage = \"PACKED-DECIMAL\", srceLine = 32)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-UXLONG\", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, byteLength = 16, isSigned = false, totalDigits = 31, picture = \"9(31)\", usage = \"PACKED-DECIMAL\", srceLine = 33)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-DEC\", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, byteLength = 5, isSigned = false, totalDigits = 9, fractionDigits = 2, picture = \"9(7)V99\", usage = \"PACKED-DECIMAL\", srceLine = 34)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-FLOAT\", type = CobolType.SINGLE_FLOAT_ITEM, levelNumber = 5, byteLength = 4, usage = \"COMP-1\", srceLine = 35)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"S-DOUBLE\", type = CobolType.DOUBLE_FLOAT_ITEM, levelNumber = 5, byteLength = 8, usage = \"COMP-2\", srceLine = 36)"));
		assertTrue(srce.contains("@CobolElement(cobolName = \"A-STRING\", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, byteLength = 4, minOccurs = 2, maxOccurs = 2, picture = \"X(4)\", usage = \"DISPLAY\", srceLine = 38)"));
	}
	
	public void testEnumAnnotation() {
		genSource("MSNSearch");
		String srce = getSource("MSNSearch", "SearchRequestType" );
	    assertTrue(srce.contains("@CobolElement(cobolName = \"SafeSearch\", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, byteLength = 32, picture = \"X(32)\", usage = \"DISPLAY\")"));
	}
	
	public void testXsdcgenOutput() {
		genSource("cultureinfo");
		String srce = getSource("cultureinfo", "CultureInfoParametersType" );
	    assertTrue(srce.contains("@CobolElement(cobolName = \"cultureCode\", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, byteLength = 32, picture = \"X(32)\", usage = \"DISPLAY\")"));
	}

	public void testXsdcgenOutputWithJavaClassNames() {
		genSource("jvmquery");
		String srce = getSource("jvmquery", "JvmQueryReplyType" );
	    assertTrue(srce.contains("@CobolComplexType(javaClassName = \"com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply\")"));
	}

	/** Generates JAXB classes with Cobol annotations */
	private void genSource(String schemaName) {
		Project project = new Project();
		project.setBasedir(".");
		DefaultLogger logger = new DefaultLogger();
		logger.setErrorPrintStream(System.out);
		project.addBuildListener(logger);
		XJC2Task xjcTask = new XJC2Task();
		xjcTask.setProject(project);
		xjcTask.setTaskName("xjcTask");
		xjcTask.setSchema(SCHEMA_DIR + '/' + schemaName + ".xsd");
		xjcTask.setDestdir(new File(GEN_SRC_DIR));
		xjcTask.setExtension(true);
		xjcTask.setRemoveOldOutput(true);
		Commandline.Argument arg1 = xjcTask.createArg();
		arg1.setValue("-Xlegstar-code");
		Commandline.Argument arg2 = xjcTask.createArg();
		arg2.setValue("-nv");
		xjcTask.log("Started");
		xjcTask.execute();
	}
	
	/** Reads a complete source file into a string. */
	private String getSource(String schemaName, String className) {
        String srcFileName = GEN_SRC_DIR + '/' + GEN_SRC_SUBDIR + '/' + schemaName + '/' + className + ".java";
		/* Read the resulting output source*/
	    try {
	    	BufferedReader in = new BufferedReader(new FileReader(srcFileName));
	        StringBuffer resStr = new StringBuffer();
	        String str = in.readLine();
	        while (str != null) {
	        	if (DEBUG_MODE) {
	        		System.out.println(str);
	        	}
	        	resStr.append(str);
	        	str = in.readLine();
	        }
	        in.close();
			return resStr.toString();
	    } catch (IOException e) {
			fail("Source file " + srcFileName + " was not generated");
			return null;
	    }
	}

}
