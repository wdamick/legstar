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
package com.legstar.coxb.gen.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import junit.framework.TestCase;
import com.legstar.coxb.gen.CoxbBindingGenerator;

public class GenerateTest extends TestCase {
	
	/** Generated files target directory. */
	private static final String GEN_DIR = "./target/java/local";
	
	/** Generated JAXB classes binaries. */
	private static final String JAXB_DIR = "./target/test-classes";
	
	/** Make sure we have an output folder. */
	protected void setUp() throws Exception {
		java.io.File td = new java.io.File(GEN_DIR);
		td.mkdirs();
	}
	
	/** Generator should check on package name. */
	public void testCheckOnPackageName() {
		try {
			CoxbBindingGenerator gen = new CoxbBindingGenerator();
			gen.execute();
		} catch (RuntimeException e) {
			assertEquals("You must specify either a JAXB package name or an XML schema file name", e.getMessage());
		}
	}
	
	/** Generator should check on target directory. */
	public void testCheckOnTarget() {
		try {
			CoxbBindingGenerator gen = new CoxbBindingGenerator();
			gen.setJaxbPackageName("com.legstar.test.coxb.vararcom");
			gen.execute();
		} catch (RuntimeException e) {
			assertEquals("You must specify a target directory", e.getMessage());
		}
	}
	
	/** Generator should check on JAXB root name. */
	public void testCheckOnJaxbRoot() {
		try {
			CoxbBindingGenerator gen = new CoxbBindingGenerator();
			gen.setJaxbPackageName("com.legstar.test.coxb.vararcom");
			gen.setTargetDir(new File(GEN_DIR));
			gen.execute();
		} catch (RuntimeException e) {
			assertEquals("You must specify a JAXB root object name", e.getMessage());
		}
	}
	
	/** Check for a successful generation. */
	public void testGenAlltypes() throws Exception  {
		CoxbBindingGenerator gen = new CoxbBindingGenerator();
		gen.setJaxbDir(new File(JAXB_DIR));
		gen.setJaxbPackageName("com.legstar.test.coxb.alltypes");
		gen.setJaxbRootObjectName("DfhcommareaType");
		gen.setTargetDir(new File(GEN_DIR));
		gen.execute();
        BufferedReader in = new BufferedReader(
        		new FileReader(
        				new File(GEN_DIR
        						+ "/com/legstar/test/coxb/alltypes/bind/"
        						+ "DfhcommareaTypeBinding.java")));
        in.readLine(); // First line is empty
        String str = in.readLine();
        assertEquals("package com.legstar.test.coxb.alltypes.bind;", str);
        in.close();
	}

	/** Check for another successful generation. */
	public void testGenDplarcht() throws Exception   {
		CoxbBindingGenerator gen = new CoxbBindingGenerator();
		gen.setJaxbDir(new File(JAXB_DIR));
		gen.setJaxbPackageName("com.legstar.test.coxb.dplarcht");
		gen.setJaxbRootObjectName("DfhcommareaType");
		gen.setTargetDir(new File(GEN_DIR));
		gen.execute();
        BufferedReader in = new BufferedReader(
        		new FileReader(
        				new File(GEN_DIR
        						+ "/com/legstar/test/coxb/dplarcht/bind/"
        						+ "DfhcommareaTypeBinding.java")));
        in.readLine(); // First line is empty
        String str = in.readLine();
        assertEquals("package com.legstar.test.coxb.dplarcht.bind;", str);
        in.close();
	}
	
}
