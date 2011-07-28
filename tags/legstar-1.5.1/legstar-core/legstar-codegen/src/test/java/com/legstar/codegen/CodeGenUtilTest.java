/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.codegen;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

/**
 * Test cases for CodeGenUtilTest.
 */
public class CodeGenUtilTest extends TestCase {

    /**
     * Check that getFile works with an absolute file name.
     * 
     * @throws Exception if test fails
     */
    public void testNoDirAbsoluteFileName() throws Exception {
        File file = new File("test.file");
        file.createNewFile();
        file.deleteOnExit();
        String dir = null;
        File file2 = CodeGenUtil.getFile(dir, file.getAbsolutePath());
        assertTrue(null != file2);
    }

    /**
     * Check that getFile works with a relative file name.
     * 
     * @throws Exception if test fails
     */
    public void testDirRelativeFileName() throws Exception {
        File dir = new File("test.dir");
        dir.mkdir();
        dir.deleteOnExit();
        File file2 = CodeGenUtil.getFile("test.dir", "test.file");
        assertTrue(file2.getAbsolutePath().contains("test.dir"));
        assertTrue(file2.getAbsolutePath().contains("test.file"));
    }

    /**
     * Check that location from pakage name works.
     * 
     * @throws Exception if test fails
     */
    public void testRelativeLocation() throws Exception {
        assertEquals("", CodeGenUtil.relativeLocation(null));
        assertEquals("/abc/", CodeGenUtil.relativeLocation("abc"));
        assertEquals("/abc/def/", CodeGenUtil.relativeLocation("abc.def"));
    }

    /**
     * Check package concatenation.
     * 
     * @throws IOException if temporary location is unusable
     */
    public void testPackageConcatenation() throws IOException {
        File dir = File.createTempFile("tempDir", "");
        dir.delete();
        dir.mkdir();
        String newDirPath = CodeGenUtil.classFilesLocation(dir,
                "com.legstar.zut", true).getAbsolutePath();
        assertEquals(dir.getAbsolutePath() + File.separator + "com"
                + File.separator + "legstar" + File.separator + "zut",
                newDirPath);
        assertTrue(new File(newDirPath).exists());
        dir.delete();
    }

    /**
     * Check field name from property name.
     * 
     * @throws Exception if test fails
     */
    public void testFieldNameFromPropertyName() throws Exception {
        assertEquals(null, CodeGenUtil.fieldNameFromPropertyName(null));
        assertEquals("a", CodeGenUtil.fieldNameFromPropertyName("A"));
        assertEquals("abc", CodeGenUtil.fieldNameFromPropertyName("Abc"));
    }

    /**
     * Check property name from field name.
     * 
     * @throws Exception if test fails
     */
    public void testPropertyNameFromFieldName() throws Exception {
        assertEquals(null, CodeGenUtil.propertyNameFromFieldName(null));
        assertEquals("A", CodeGenUtil.propertyNameFromFieldName("a"));
        assertEquals("Abc", CodeGenUtil.propertyNameFromFieldName("abc"));
    }

    /**
     * Check property name from JAXB name.
     * 
     * @throws Exception if test fails
     */
    public void testPropertyNameFromJaxbType() throws Exception {
        assertEquals(null, CodeGenUtil.propertyNameFromJaxbType(null));
        assertEquals("A", CodeGenUtil.propertyNameFromJaxbType("A"));
        assertEquals("A", CodeGenUtil.propertyNameFromJaxbType("AType"));
    }
}
