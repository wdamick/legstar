/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.codegen;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


import junit.framework.TestCase;

/**
 * Test cases for CixsMake.
 */
public class CodeGenMakeTest extends TestCase {

    /** Logger. */
    private final Log _log = LogFactory.getLog(CodeGenMakeTest.class);

    /** Tests will store generated files here. */
    private static final String WORK_FOLDER = "target/gen";

    /**
     * Check controls on input make file.
     */
    public void testCixsMakeInputValidation() {
        CodeGenMake codeGenMake = new CodeGenMake();
        codeGenMake.setModelName("modelName");
        codeGenMake.setModel("model");
        try {
            codeGenMake.execute();
        } catch (RuntimeException e) {
            assertEquals("Missing make file parameter", e.getMessage());
        }
        codeGenMake.setCodeGenMakeFileName("tarass.boulba");
        try {
            codeGenMake.execute();
        } catch (RuntimeException e) {
            assertEquals("Code generation make file tarass.boulba does not exist", e.getMessage());
        }
    }

    /**
     * Check controls on input make file tag &lt;cixstarget&gt;.
     * @throws IOException if file cannot be read
     */
    public void testCodeGenMakeNoTargetTag() throws IOException {
        File tempMakeFile = File.createTempFile("test-temp", "xml");
        /* Create a temporary make file */
        BufferedWriter out;
        out = new BufferedWriter(new FileWriter(tempMakeFile));
        out.write("<somethingElse/>");
        out.close();

        CodeGenMake codeGenMake = new CodeGenMake();
        codeGenMake.setModelName("modelName");
        codeGenMake.setModel("model");
        codeGenMake.setCodeGenMakeFileName(tempMakeFile.getPath());
        try {
            codeGenMake.execute();
        } catch (RuntimeException e) {
            assertEquals("Empty or invalid code generation make file", e.getMessage());
        }
    }

    /**
     * Check controls on input make file tag &lt;cixstarget name=""&gt;.
     * @throws IOException if file cannot be read
     */
    public void testCodeGenMakeNoTargetName() throws IOException {
        File tempMakeFile = File.createTempFile("test-temp", "xml");
        /* Create a temporary make file */
        BufferedWriter out;
        out = new BufferedWriter(new FileWriter(tempMakeFile));
        out.write("<target/>");
        out.close();

        CodeGenMake codeGenMake = new CodeGenMake();
        codeGenMake.setModelName("modelName");
        codeGenMake.setModel("model");
        codeGenMake.setCodeGenMakeFileName(tempMakeFile.getPath());
        try {
            codeGenMake.execute();
        } catch (RuntimeException e) {
            assertEquals("Missing name attribute for target element", e.getMessage());
        }
    }

    /**
     * Check controls on input make file tag &lt;cixstemplate name=""&gt;.
     * @throws IOException if file cannot be read
     */
    public void testCodeGenMakeTemplateNoTemplateName() throws IOException {
        File tempMakeFile = File.createTempFile("test-temp", "xml");
        /* Create a temporary make file */
        BufferedWriter out;
        out = new BufferedWriter(new FileWriter(tempMakeFile));
        out.write("<target name=\"aTarget\"><step/></target>");
        out.close();

        CodeGenMake codeGenMake = new CodeGenMake();
        codeGenMake.setModelName("modelName");
        codeGenMake.setModel("model");
        codeGenMake.setCodeGenMakeFileName(tempMakeFile.getPath());
        try {
            codeGenMake.execute();
        } catch (RuntimeException e) {
            assertEquals(
                    "Missing template name attribute for step element",
                    e.getMessage());
        }
    }

    /**
     * Check controls on input make file tag &lt;step targetFile=""&gt;.
     * @throws IOException if file cannot be read
     */
    public void testCodeGenMakeTemplateNoTargetFileName() throws IOException {
        File tempMakeFile = File.createTempFile("test-temp", "xml");
        /* Create a temporary make file */
        BufferedWriter out;
        out = new BufferedWriter(new FileWriter(tempMakeFile));
        out.write(
                "<target name=\"aTarget\"><step templateName=\"tt.vm\"/>"
                +  "</target>");
        out.close();

        CodeGenMake codeGenMake = new CodeGenMake();
        codeGenMake.setModelName("modelName");
        codeGenMake.setModel("model");
        codeGenMake.setCodeGenMakeFileName(tempMakeFile.getPath());
        try {
            codeGenMake.execute();
        } catch (RuntimeException e) {
            assertEquals(
                    "Missing template target file name attribute for"
                    + " step element",
                    e.getMessage());
        }
    }

    /**
     * Check generation.
     * @throws IOException if file cannot be read
     */
    public void testCodeGenMakeTemplateWithParameters() throws IOException {
        File tempMakeFile = File.createTempFile("test-temp", "xml");
        /* Create a temporary make file */
        BufferedWriter out;
        out = new BufferedWriter(new FileWriter(tempMakeFile));
        out.write("<target name=\"aTarget\" dir=\"" + WORK_FOLDER + "\">"
                + "<step templateName=\"testtemplate.vm\" targetFile=\"test.text\">"
                + "<parm1 value=\"value1\"/><parm2 value=\"value2\"/>"
                + "</step></target>");
        out.close();

        CodeGenMake codeGenMake = new CodeGenMake();
        codeGenMake.init();
        codeGenMake.setModelName("modelName");
        codeGenMake.setModel("model");
        codeGenMake.setCodeGenMakeFileName(tempMakeFile.getPath());
        codeGenMake.execute();
        BufferedReader in = new BufferedReader(new FileReader(WORK_FOLDER + "/test.text"));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            _log.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        assertTrue(resStr.contains("Using value1 and value2"));
    }

    /**
     * Check generation with a requested character set.
     * @throws IOException if file cannot be read
     */
    public void testCodeGenMakeTemplateWithParametersAndCharset() throws IOException {
        File tempMakeFile = File.createTempFile("test-temp", "xml");
        /* Create a temporary make file */
        BufferedWriter out;
        out = new BufferedWriter(new FileWriter(tempMakeFile));
        out.write("<target name=\"aTarget\" dir=\"" + WORK_FOLDER + "\">"
                + "<step templateName=\"testtemplate.vm\""
                + " targetFile=\"test-utf8.text\" targetCharsetName=\"UTF-8\">"
                + "<parm1 value=\"value1\"/><parm2 value=\"value2\"/>"
                + "</step></target>");
        out.close();

        CodeGenMake codeGenMake = new CodeGenMake();
        codeGenMake.init();
        codeGenMake.setModelName("modelName");
        codeGenMake.setModel("model");
        codeGenMake.setCodeGenMakeFileName(tempMakeFile.getPath());
        codeGenMake.execute();
        BufferedReader in = new BufferedReader(new InputStreamReader(
                new FileInputStream(WORK_FOLDER + "/test-utf8.text"), "UTF-8"));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            _log.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        assertTrue(resStr.contains("Using value1 and value2"));
    }
}
