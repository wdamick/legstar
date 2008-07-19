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
package com.legstar.coxb.misc.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import com.legstar.coxb.gen.CoxbBindingGenerator;

import junit.framework.TestCase;

public class EnumgenCases extends TestCase {

	private static final boolean DEBUG_MODE = true;
	private static final String GEN_SRC_DIR = "./target/java/local";
	private static final String GEN_SRC_SUBDIR = "com/legstar/test/coxb";
	
	/** Generated JAXB classes binaries. */
	private static final String JAXB_DIR = "../legstar-jaxbgen/target/gen-classes";
	
	/** Make sure we have an output folder. */
	protected void setUp() throws Exception {
		java.io.File td = new java.io.File(GEN_SRC_DIR);
		td.mkdirs();
	}

	/** Check for a successful generation. */
	public void testGenEnumTypes() throws Exception  {
		CoxbBindingGenerator gen = new CoxbBindingGenerator();
		gen.setJaxbBinDir(new File(JAXB_DIR));
		gen.setJaxbPackageName("com.legstar.test.coxb.enumvar");
		gen.setJaxbRootClassName("SearchRequestType");
		gen.setTargetDir(new File(GEN_SRC_DIR));
		gen.execute();
		String srce = getSource("enumtypes", "SearchRequestType" );
        assertTrue(srce.contains("package com.legstar.test.coxb.enumtypes.bind;"));
 	}
	
	/** Reads a complete source file into a string. */
	private String getSource(String schemaName, String className) {
        String srcFileName = GEN_SRC_DIR + '/' + GEN_SRC_SUBDIR + '/' + schemaName + "/bind/" + className + "Binding.java";
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
