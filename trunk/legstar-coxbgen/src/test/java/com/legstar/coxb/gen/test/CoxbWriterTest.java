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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import junit.framework.TestCase;

import com.legstar.coxb.gen.CoxbWriter;
import com.legstar.coxb.host.HostException;

public class CoxbWriterTest extends TestCase {
	
	/** Generated files target directory. */
	private static final String GEN_DIR = "./target/java/local";
	
	
	/** Make sure we have an output folder. */
	protected void setUp() throws Exception {
		java.io.File td = new java.io.File(GEN_DIR);
		td.mkdirs();
	}
	
	/** Writer must check if target dir exists. */
	public void testDirNonExistant() throws HostException {
		
		try {
			new CoxbWriter(new File("bidon"));
			fail("Non existant test failed ");
		} catch (Exception e) {
			assertEquals("bidon does not exist", e.getMessage());
		}
		
	}

	/** Writer must check if target dir is a dir. */
	public void testDirNotADirectory() throws HostException {
		
		try {
			new CoxbWriter(
					new File("src/test/resources/alltypes.xsd"));
			fail("Non existant test failed");
		} catch (Exception e) {
			assertEquals("src\\test\\resources\\alltypes.xsd is not a directory", e.getMessage());
		}
		
	}

	/** Writer must be ok with existing dir. */
	public void testDirExistant() throws HostException {
		
		try {
			CoxbWriter cw = new CoxbWriter(new File(GEN_DIR));
			assertEquals("class com.legstar.coxb.gen.CoxbWriter", cw.getClass().toString());
		} catch (Exception e) {
			fail("Existant test failed");
		}
		
	}
	
	/** Writer must create a temporary file with the given content. */
	public void testNewFile1() {
		try {
			CoxbWriter cw = new CoxbWriter(new File(GEN_DIR));
			String fn = cw.openWrite("spaghetti al arrabiata");
			cw.closeAll();
	        BufferedReader in = new BufferedReader(new FileReader(new File(fn)));
	        String str = in.readLine();
	        in.close();
			assertEquals("spaghetti al arrabiata", str);
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	/** Writer should be able to handle a stach of temporary files. */
	public void testTwoNewFiles() {
		try {
			CoxbWriter cw = new CoxbWriter(new File(GEN_DIR));
			String fn1 = cw.openWrite("spaghetti al arrabiata");
			String fn2 = cw.openWrite("spaghetti al limone");
			cw.closeAll();
	        BufferedReader in1 = new BufferedReader(new FileReader(new File(fn1)));
	        String str1 = in1.readLine();
	        in1.close();
			assertEquals("spaghetti al arrabiata", str1);
	        BufferedReader in2 = new BufferedReader(new FileReader(new File(fn2)));
	        String str2 = in2.readLine();
	        in2.close();
			assertEquals("spaghetti al limone", str2);
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	/** Additional content must use the top (last) temporary file in the stack.*/
	public void testTwoNewFilesWithContent() {
		try {
			CoxbWriter cw = new CoxbWriter(new File(GEN_DIR));
			cw.openWrite("spaghetti al arrabiata");
			String fn2 = cw.openWrite("spaghetti al limone");
			String fn3 = cw.write(" y al burro");
			cw.closeAll();
			assertEquals(fn2, fn3);
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	/** Check that a close operation pops the temporary file from the stack. */
	public void testCloseOperation() {
		try {
			CoxbWriter cw = new CoxbWriter(new File(GEN_DIR));
			String fn1 = cw.openWrite("spaghetti al arrabiata");
			String fn2 = cw.writeClose(" y al pomodoro");
			assertEquals(fn1, fn2);
			assertEquals(0, cw.getFileStack().size());
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	/** Chack that an attempt to write without prior open fails. */
	public void testWriteOnEmptyStack() {
		try {
			CoxbWriter cw = new CoxbWriter(new File(GEN_DIR));
			cw.writeClose(" y al pomodoro");
			fail("write on empty stack test failed");
		} catch (Exception e) {
			assertEquals("No current open file on stack", e.getMessage());;
		}
	}
}
