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
package com.legstar.schemagen.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenUtil;
import com.legstar.schemagen.CobolToXsdCobolModel;

import junit.framework.TestCase;

/**
 * This tests the generation of an ant script which in turn, generates a schema.
 *
 */
public class CobolToXsdCobolModelTest extends TestCase {


    /** Logger. */
    private static final Log LOG = LogFactory.getLog(CobolToXsdCobolModelTest.class);

    /** Ant scripts files will be generated here. */
    public static final File GEN_ANT_DIR = new File("target/src/gen/ant");

    /** Make sure we have an output folder.
     * @throws Exception if output folder cannot be created */
    protected void setUp() throws Exception {
        CodeGenUtil.checkDirectory(GEN_ANT_DIR, true);
    }
    /**
     * Generate and check ant script.
     * @throws Exception if test fails
     */
    public void testBuilc2sc() throws Exception {

        CobolToXsdCobolModel model = new CobolToXsdCobolModel();

        model.setProductLocation("/Users/Fady/sandbox/legstar-version");
        model.setSourceCobolFilePath("src/main/resources/cobol/LSFILEAE.CBL");
        model.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
        model.setJaxbTypeClassesSuffix("TypeSuffix");
        model.setNamespace("http://lsfileae.cases.test.schemagen.legstar.com/");
        model.setTargetDir(new File("src/test/gen/schema"));
        model.setTargetXsdFileName("lsfileae.xsd");
        model.setProbeFile(new File("probe.file.tmp"));

        model.generateBuild(CodeGenUtil.getFile(GEN_ANT_DIR, "test.txt"));

        BufferedReader in = new BufferedReader(new FileReader(new File(GEN_ANT_DIR, "/test.txt")));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            LOG.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        assertTrue(resStr.contains("<project basedir=\"/Users/Fady/sandbox/legstar-version\""
                + " default=\"signalSuccess\" name=\"generate-XSD\">"));
        assertTrue(resStr.contains("<echo message=\"Generating annotated XML schema lsfileae.xsd\" />"));
        assertTrue(resStr.contains("namespace=\"http://lsfileae.cases.test.schemagen.legstar.com/\""));
        assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.lsfileae\""));
        assertTrue(resStr.contains("jaxbTypeClassesSuffix=\"TypeSuffix\""));
        assertTrue(resStr.contains("sourceCobolFilePath=\"src/main/resources/cobol/LSFILEAE.CBL\""));
        assertTrue(resStr.contains("targetDir=\"src\\test\\gen\\schema\""));
        assertTrue(resStr.contains("targetXsdFileName=\"lsfileae.xsd\""));
        assertTrue(resStr.contains("<delete file=\"probe.file.tmp\" quiet=\"true\"/>"));
    }

}
