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
package com.legstar.codegen;

import java.io.File;

import junit.framework.TestCase;

/**
 * Test cases for CodeGenUtilTest.
 */
public class CodeGenUtilTest extends TestCase {

	/**
	 * Check that getFile works with an absolute file name.
	 * @throws Exception if test fails
	 */
	public final void testNoDirAbsoluteFileName() throws Exception {
		File file = new File("test.file");
		file.createNewFile();
		file.deleteOnExit();
		String dir = null;
		File file2 = CodeGenUtil.getFile(dir, file.getAbsolutePath());
		assertTrue(null != file2);
	}

	/**
	 * Check that getFile works with a relative file name.
	 * @throws Exception if test fails
	 */
	public final void testDirRelativeFileName() throws Exception {
		File dir = new File("test.dir");
		dir.mkdir();
		dir.deleteOnExit();
		File file2 = CodeGenUtil.getFile("test.dir", "test.file");
		assertTrue(file2.getAbsolutePath().contains("test.dir"));
		assertTrue(file2.getAbsolutePath().contains("test.file"));
	}

	/**
	 * Check that class normalization works.
	 * @throws Exception if test fails
	 */
	public final void testClassNormalization() throws Exception {
		assertEquals(null, CodeGenUtil.classNormalize(null));
		assertEquals("A", CodeGenUtil.classNormalize("a"));
		assertEquals("Abc", CodeGenUtil.classNormalize("abc"));
	}

	/**
	 * Check that location from pakage name works.
	 * @throws Exception if test fails
	 */
	public final void testRelativeLocation() throws Exception {
		assertEquals("", CodeGenUtil.relativeLocation(null));
		assertEquals("/abc/", CodeGenUtil.relativeLocation("abc"));
		assertEquals("/abc/def/", CodeGenUtil.relativeLocation("abc.def"));
	}
	
	public final void testPackageConcatenation() {
		File dir = new File("c:\\root\\com\\legstar\\zut");
		dir.delete();
		assertEquals("c:\\root\\com\\legstar\\zut",
				CodeGenUtil.classFilesLocation(new File("c:/root"),
						"com.legstar.zut").getAbsolutePath());
		dir = new File("c:\\root\\com\\legstar\\zut");
		assertTrue(dir.exists());
	}

}
