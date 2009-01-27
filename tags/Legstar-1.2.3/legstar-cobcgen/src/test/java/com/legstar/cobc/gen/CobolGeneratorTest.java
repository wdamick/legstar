/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobc.gen;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;


/**
 * Test CobolGenerator.
 *
 */
public class CobolGeneratorTest extends AbstractTester {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(CobolGeneratorTest.class);
    /**
     * Test instantiation.
     */
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

    /**
     * Check classpath issues.
     */
    public void testClasspathIssues() {
        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("dfhcommarea");
            gen.setTargetDir(GEN_DIR);
            gen.execute();
            fail();
        } catch (BuildException e) {
            assertEquals("java.lang.ClassNotFoundException: dfhcommarea", e.getCause().getCause().getMessage());
        }

        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("dfhcommareaTruc");
            gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
            gen.setTargetDir(GEN_DIR);
            gen.execute();
            fail();
        } catch (BuildException e) {
            assertEquals("java.lang.ClassNotFoundException: com.legstar.test.coxb.lsfileae.dfhcommareaTruc",
                    e.getCause().getCause().getMessage());
        }
    }

    /**
     * Perform straight generation.
     */
    public void testGenerate() {
        /* Use default root name */
        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("Dfhcommarea");
            gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
            gen.setTargetDir(GEN_DIR);
            gen.execute();
            String source = getSource(GEN_DIR, "Dfhcommarea.cbl");
            assertTrue(source.contains("01 Dfhcommarea."));
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
            gen.setJaxbTypeName("Dfhcommarea");
            gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
            gen.setTargetDir(GEN_DIR);
            gen.setCobolRootDataItemName("COM-LSFILEAE");
            gen.execute();
            String source = getSource(GEN_DIR, "Dfhcommarea.cbl");
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
            gen.setJaxbTypeName("Dfhcommarea");
            gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
            gen.setTargetDir(GEN_DIR);
            gen.setCobolRootDataItemName("COM-LSFILEAE");
            gen.setTargetCobolFileName("lsfileae.cpy");
            gen.execute();
            String source = getSource(GEN_DIR, "lsfileae.cpy");
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

    /**
     * @throws Exception
     */
    public void testGenerateCultureInfo() {
        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("CultureInfoParameters");
            gen.setJaxbPackageName("com.legstar.test.coxb.cultureinfo");
            gen.setTargetDir(GEN_DIR);
            gen.setCobolRootDataItemName("COM-REQUEST");
            gen.setTargetCobolFileName("cultureinfo-request.cpy");
            gen.execute();
            String source = getSource(GEN_DIR, "cultureinfo-request.cpy");
            assertTrue(source.contains("COM-REQUEST."));
            gen.setJaxbTypeName("CultureInfoReply");
            gen.setCobolRootDataItemName("COM-REPLY");
            gen.setTargetCobolFileName("cultureinfo-reply.cpy");
            gen.execute();
            String source2 = getSource(GEN_DIR, "cultureinfo-reply.cpy");
            assertTrue(source2.contains("COM-REPLY."));
        } catch (IOException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Test the generate method directly.
     */
    public void testGenerateDirect() {
        try {
            String code = CobolGenerator.generate(
                    "com.legstar.test.coxb.lsfileae",
                    "Dfhcommarea",
                    "COM-LSFILEAE",
                    5,
                    5);
            LOG.debug(code);
            assertEquals(
                    /* 123456789012345678901234567890123456789012345678901234567890123456789012*/
                      "           05 COM-LSFILEAE." + "\r\n"
                    + "               10 COM-NUMBER PIC 9(6)." + "\r\n"
                    + "               10 COM-PERSONAL." + "\r\n"
                    + "                   15 COM-NAME PIC X(20)." + "\r\n"
                    + "                   15 COM-ADDRESS PIC X(20)." + "\r\n"
                    + "                   15 COM-PHONE PIC X(8)." + "\r\n"
                    + "               10 COM-DATE PIC X(8)." + "\r\n"
                    + "               10 COM-AMOUNT PIC X(8)." + "\r\n"
                    + "               10 COM-COMMENT PIC X(9)." + "\r\n", code);

            code = CobolGenerator.generate(
                    "com.legstar.test.coxb.lsfileae",
                    "Dfhcommarea",
                    "COM-LSFILEAE",
                    1,
                    1);
            LOG.debug(code);
            assertEquals(
                    /* 123456789012345678901234567890123456789012345678901234567890123456789012*/
                      "       01 COM-LSFILEAE." + "\r\n"
                    + "           02 COM-NUMBER PIC 9(6)." + "\r\n"
                    + "           02 COM-PERSONAL." + "\r\n"
                    + "               03 COM-NAME PIC X(20)." + "\r\n"
                    + "               03 COM-ADDRESS PIC X(20)." + "\r\n"
                    + "               03 COM-PHONE PIC X(8)." + "\r\n"
                    + "           02 COM-DATE PIC X(8)." + "\r\n"
                    + "           02 COM-AMOUNT PIC X(8)." + "\r\n"
                    + "           02 COM-COMMENT PIC X(9)." + "\r\n", code);

            code = CobolGenerator.generate(
                    "com.legstar.test.coxb.lsfileae",
                    "Dfhcommarea",
                    "COM-LSFILEAE",
                    2,
                    1);
            LOG.debug(code);
            assertEquals(
                    /* 123456789012345678901234567890123456789012345678901234567890123456789012*/
                      "           02 COM-LSFILEAE." + "\r\n"
                    + "               03 COM-NUMBER PIC 9(6)." + "\r\n"
                    + "               03 COM-PERSONAL." + "\r\n"
                    + "                   04 COM-NAME PIC X(20)." + "\r\n"
                    + "                   04 COM-ADDRESS PIC X(20)." + "\r\n"
                    + "                   04 COM-PHONE PIC X(8)." + "\r\n"
                    + "               03 COM-DATE PIC X(8)." + "\r\n"
                    + "               03 COM-AMOUNT PIC X(8)." + "\r\n"
                    + "               03 COM-COMMENT PIC X(9)." + "\r\n", code);
            code = CobolGenerator.generate(
                    "com.legstar.test.coxb.lsfileae",
                    "Dfhcommarea",
                    "COM-LSFILEAE",
                    1,
                    2);
            LOG.debug(code);
            assertEquals(
                    /* 123456789012345678901234567890123456789012345678901234567890123456789012*/
                      "       01 COM-LSFILEAE." + "\r\n"
                    + "           03 COM-NUMBER PIC 9(6)." + "\r\n"
                    + "           03 COM-PERSONAL." + "\r\n"
                    + "               05 COM-NAME PIC X(20)." + "\r\n"
                    + "               05 COM-ADDRESS PIC X(20)." + "\r\n"
                    + "               05 COM-PHONE PIC X(8)." + "\r\n"
                    + "           03 COM-DATE PIC X(8)." + "\r\n"
                    + "           03 COM-AMOUNT PIC X(8)." + "\r\n"
                    + "           03 COM-COMMENT PIC X(9)." + "\r\n", code);
        } catch (CobolGenerationException e) {
            fail(e.getMessage());
        }
    }

}
