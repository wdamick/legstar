/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.gen;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;

import com.legstar.codegen.CodeGenUtil;

/**
 * Generate the ant scripts that will be delivered as samples
 * in the distribution.
 * 
 */
public class CoxbGenAntSamplesTest extends AbstractTestTemplate {

    /** Product location relative to where the sample ant gets distributed.. */
    private static final String SAMPLES_PRODUCT_LOCATION = "../../..";
    /** We want ant scripts generated here. */
    private static final File SAMPLES_GEN_ANT_DIR = new File("target/ant");
    /** Samples will take sources from here. */
    private static final File SAMPLES_GEN_SRC_DIR = new File("src");
    /** Samples will produce binaries here. */
    private static final File SAMPLES_GEN_BIN_DIR = new File("bin");

    /** The parameters set. */
    private CoxbGenModel _model;

    /** @{inheritDoc */
    public void setUp() {
        super.setUp();
        _model = new CoxbGenModel();
    }

    /**
     * Generate the sample ant scripts.
     * 
     * @throws Exception if ant cannot be generated
     */
    public void testBuildCoxb() throws Exception {

        List < String > jaxbRootClassNames = new ArrayList < String >();
        File script = null;

        jaxbRootClassNames.clear();
        jaxbRootClassNames.add("Dfhcommarea");
        script = buildCoxbAntScript("lsfileae",
                "com.legstar.test.coxb.lsfileae", jaxbRootClassNames);
        assertNotNull(script);

        jaxbRootClassNames.clear();
        jaxbRootClassNames.add("JvmQueryRequest");
        jaxbRootClassNames.add("JvmQueryReply");
        script = buildCoxbAntScript("jvmquery",
                "com.legstar.test.coxb.jvmquery", jaxbRootClassNames);
        assertNotNull(script);

        jaxbRootClassNames.clear();
        jaxbRootClassNames.add("GetInfo");
        jaxbRootClassNames.add("GetInfoResponse");
        script = buildCoxbAntScript("cultureinfo",
                "com.legstar.test.coxb.cultureinfo", jaxbRootClassNames);
        assertNotNull(script);

    }

    /**
     * Generate an ant script capable of producing the binding artifacts using
     * the VLC template.
     * 
     * @param xsdFileName the XSD file name
     * @param jaxbPackageName the JAXB package name
     * @param jaxbRootClassNames list of root class names
     * @throws Exception if ant cannot be generated
     * @return the ant script file
     */
    public File buildCoxbAntScript(
            final String xsdFileName,
            final String jaxbPackageName,
            final List < String > jaxbRootClassNames) throws Exception {

        _model.setProductLocation(SAMPLES_PRODUCT_LOCATION);
        _model.setProbeFile(new File("probe.file.tmp"));
        _model.setJaxbSrcDir(SAMPLES_GEN_SRC_DIR);
        _model.setJaxbBinDir(SAMPLES_GEN_BIN_DIR);
        _model.setXsdFile(new File("schema/" + xsdFileName + ".xsd"));
        _model.setCoxbSrcDir(SAMPLES_GEN_SRC_DIR);
        _model.setCoxbBinDir(SAMPLES_GEN_BIN_DIR);
        _model.setJaxbRootClassNames(jaxbRootClassNames);
        _model.setJaxbPackageName(jaxbPackageName);

        // Sample ant scripts must compile the COXB classes
        _model.setCompileTransformers(true);

        return genAntScriptAsFile(xsdFileName);

    }

    /**
     * Generates an ant script from a VLC template.
     * 
     * @param xsdFileName used to prevent output from different samples
     *            to override others
     * @return the script as a string
     * @throws Exception if generation fails
     */
    protected File genAntScriptAsFile(final String xsdFileName)
            throws Exception {
        CodeGenUtil.checkDirectory(SAMPLES_GEN_ANT_DIR + "/" + xsdFileName,
                true);
        File resultFile = CodeGenUtil.getFile(SAMPLES_GEN_ANT_DIR, xsdFileName
                + "/build-coxb.xml");
        _model.generateBuild(resultFile);
        return deMicrosoftFileNames(resultFile);
    }

    /**
     * We don't want our samples to contain dataset names with
     * the Microsoft specific backslash as the file separator.
     * 
     * @param file the file which content should be changed
     * @return a file with new content
     * @throws Exception if something goes wrong
     */
    protected File deMicrosoftFileNames(final File file) throws Exception {
        String content = FileUtils.readFileToString(file);
        content = content.replace('\\', '/');
        FileUtils.writeStringToFile(file, content);
        return file;

    }

}
