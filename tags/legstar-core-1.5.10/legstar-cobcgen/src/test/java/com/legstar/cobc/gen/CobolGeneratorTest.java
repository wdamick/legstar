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
package com.legstar.cobc.gen;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.tools.ant.BuildException;

import com.legstar.cobc.AbstractTest;

/**
 * Test CobolGenerator.
 * 
 */
public class CobolGeneratorTest extends AbstractTest {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    public boolean isCreateReferences() {
        return CREATE_REFERENCES;
    }

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
            assertEquals("java.lang.ClassNotFoundException: ObjectFactory", e
                    .getCause().getCause().getMessage());
        }

        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("dfhcommareaTruc");
            gen.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
            gen.setTargetDir(GEN_DIR);
            gen.execute();
            fail();
        } catch (BuildException e) {
            assertEquals(
                    "java.lang.ClassNotFoundException: com.legstar.test.coxb.lsfileae.dfhcommareaTruc",
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
            gen.setCobolRootDataItemName("COM-LSFILEAE");
            gen.execute();
            check(FileUtils.readFileToString(new File(GEN_DIR,
                    "Dfhcommarea.cbl")));
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
            check(FileUtils.readFileToString(new File(GEN_DIR,
                    "Dfhcommarea.cbl")));
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
            check(FileUtils.readFileToString(new File(GEN_DIR, "lsfileae.cpy")));
        } catch (BuildException e) {
            fail(e.getMessage());
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * @throws Exception
     */
    public void testGenerateCultureInfoRequest() {
        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("CultureInfoParameters");
            gen.setJaxbPackageName("com.legstar.test.coxb.cultureinfo");
            gen.setTargetDir(GEN_DIR);
            gen.setCobolRootDataItemName("COM-REQUEST");
            gen.setTargetCobolFileName("cultureinfo-request.cpy");
            gen.execute();
            check(FileUtils.readFileToString(new File(GEN_DIR,
                    "cultureinfo-request.cpy")));
        } catch (IOException e) {
            fail(e.getMessage());
        }

    }

    /**
     * @throws Exception
     */
    public void testGenerateCultureInfoReply() {
        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("CultureInfoReply");
            gen.setJaxbPackageName("com.legstar.test.coxb.cultureinfo");
            gen.setTargetDir(GEN_DIR);
            gen.setCobolRootDataItemName("COM-REPLY");
            gen.setTargetCobolFileName("cultureinfo-reply.cpy");
            gen.execute();
            check(FileUtils.readFileToString(new File(GEN_DIR,
                    "cultureinfo-reply.cpy")));
        } catch (IOException e) {
            fail(e.getMessage());
        }

    }

    /**
     * @throws Exception
     */
    public void testGenerateDplarcht() {
        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("Dfhcommarea");
            gen.setJaxbPackageName("com.legstar.test.coxb.dplarcht");
            gen.setTargetDir(GEN_DIR);
            gen.setCobolRootDataItemName("DFHCOMMAREA");
            gen.setTargetCobolFileName("dplarcht.cpy");
            gen.execute();
            check(FileUtils.readFileToString(new File(GEN_DIR, "dplarcht.cpy")));
        } catch (IOException e) {
            fail(e.getMessage());
        }

    }

    /**
     * @throws Exception
     */
    public void testGenerateMSNSearch() {
        try {
            CobolGenerator gen = new CobolGenerator();
            gen.setJaxbTypeName("Search");
            gen.setJaxbPackageName("com.legstar.test.coxb.MSNSearch");
            gen.setTargetDir(GEN_DIR);
            gen.setCobolRootDataItemName("COM-MSNSEARCH");
            gen.setTargetCobolFileName("msnsearch.cpy");
            gen.execute();
            check(FileUtils
                    .readFileToString(new File(GEN_DIR, "msnsearch.cpy")));
        } catch (IOException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Test the generate method directly.
     * 
     * @throws CobolGenerationException if generation fails
     */
    public void testGenerateDirectFrom5Incr5() throws CobolGenerationException {
        String code = CobolGenerator.generate("com.legstar.test.coxb.lsfileae",
                "Dfhcommarea", "COM-LSFILEAE", 5, 5);
        check(code);
    }

    /**
     * Test the generate method directly.
     * 
     * @throws CobolGenerationException if generation fails
     */
    public void testGenerateDirectFrom1Incr1() throws CobolGenerationException {
        String code = CobolGenerator.generate("com.legstar.test.coxb.lsfileae",
                "Dfhcommarea", "COM-LSFILEAE", 1, 1);
        check(code);
    }

    /**
     * Test the generate method directly.
     * 
     * @throws CobolGenerationException if generation fails
     */
    public void testGenerateDirectFrom2Incr1() throws CobolGenerationException {
        String code = CobolGenerator.generate("com.legstar.test.coxb.lsfileae",
                "Dfhcommarea", "COM-LSFILEAE", 2, 1);
        check(code);
    }

    /**
     * Test the generate method directly.
     * 
     * @throws CobolGenerationException if generation fails
     */
    public void testGenerateDirectFrom1Incr2() throws CobolGenerationException {
        String code = CobolGenerator.generate("com.legstar.test.coxb.lsfileae",
                "Dfhcommarea", "COM-LSFILEAE", 1, 2);
        check(code);
    }

}
