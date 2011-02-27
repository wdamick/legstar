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
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;

/**
 * Test the binding generator.
 * 
 */
public class CoxbBindingGeneratorTest extends AbstractCoxbGenTest {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /** List of XSDs which need special generation parameters. */
    private static final List < String > NON_STANDARD_XSDS = Arrays
            .asList(new String[] { "LSFILEAL.xsd", "LSFILEAC.xsd",
                    "enumvar.xsd", "MSNSearch.xsd", "cultureinfo.xsd",
                    "jvmquery.xsd", "jvmquery-ws.xsd", "VARAR021.xsd",
                    "TCOBWVB.xsd", "RQ071CICSECIBinding.xsd" });

    /** Make sure we have a clean output folder. */
    public void setUp() throws Exception {
        super.setUp();
        setCreateReferences(CREATE_REFERENCES);
    }

    /** Generator should check on package name. */
    public void testCheckOnPackageName() {
        try {
            CoxbBindingGenerator gen = new CoxbBindingGenerator();
            gen.execute();
        } catch (RuntimeException e) {
            assertEquals(
                    "You must specify either a JAXB package name or an XML schema file name",
                    e.getMessage());
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
            gen.setTargetDir(GEN_SRC_DIR);
            gen.execute();
        } catch (RuntimeException e) {
            assertEquals("You must specify at least one JAXB root class name",
                    e.getMessage());
        }
    }

    /**
     * Test all standard cases.
     * 
     * @throws Exception if generation fails
     */
    @SuppressWarnings("unchecked")
    public void testAllStandard() throws Exception {
        Collection < File > xsdFiles = FileUtils.listFiles(COB_XSD_DIR,
                new String[] { "xsd" }, false);
        for (File schemaFile : xsdFiles) {
            if (!NON_STANDARD_XSDS.contains(schemaFile.getName())) {
                genSourceAndCheck(schemaFile, "Dfhcommarea");
            }
        }
    }

    /**
     * Test LSFILEAL.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileal() throws Exception {
        genSourceAndCheck(new File(COB_XSD_DIR, "LSFILEAL.xsd"), new String[] {
                "RequestParms", "ReplyData" });
    }

    /**
     * Test LSFILEAC.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileac() throws Exception {
        genSourceAndCheck(new File(COB_XSD_DIR, "LSFILEAC.xsd"), new String[] {
                "QueryData", "QueryLimit", "ReplyStatus", "ReplyData" });
    }

    /**
     * Generate binding for Enumvar.
     * <p/>
     * This was kept separate because there is no COBOL CICS program for this.
     * (This is a fragment of MSNSearch).
     * 
     * @throws Exception if generation fails
     */
    public void testGenEnumvar() throws Exception {
        genSourceAndCheck("enumvar", "SearchRequestType");
    }

    /**
     * Generate binding for MSNSearch.
     * 
     * @throws Exception if generation fails
     */
    public void testMSNSearch() throws Exception {
        genSourceAndCheck("MSNSearch", new String[] { "Search",
                "SearchResponse" });
    }

    /**
     * Check that we can provide multiple class names at once.
     * 
     * @throws Exception if generation fails
     */
    public void testCultureInfo() throws Exception {
        genSourceAndCheck("cultureinfo", new String[] {
                "CultureInfoParameters", "CultureInfoReply" });
    }

    /**
     * Generate binding for JvmQuery.
     * 
     * @throws Exception if generation fails
     */
    public void testJvmQuery() throws Exception {
        genSourceAndCheck("jvmquery", new String[] { "JvmQueryRequest",
                "JvmQueryReply" });
    }

    /**
     * Generate binding for JvmQuery Web Service.
     * 
     * @throws Exception if generation fails
     */
    public void testJvmQueryWs() throws Exception {
        genSourceAndCheck("ws.jvmquery", new String[] { "QueryJvm",
                "QueryJvmResponse" });
    }

    /**
     * Generate binding for Varar021.
     * 
     * @throws Exception if generation fails
     */
    public void testVarar021() throws Exception {
        genSourceAndCheck("varar021", "SearchGrplst");
    }

    /**
     * Generate binding for tcobwvb.
     * 
     * @throws Exception if generation fails
     */
    public void testTcobwvb() throws Exception {
        genSourceAndCheck(new File(COB_XSD_DIR, "TCOBWVB.xsd"), "CustomerData");
    }

    /**
     * Generate an ECI compatible transformer.
     * 
     * @throws Exception
     */
    public void testRq071() throws Exception {
        genSourceAndCheck("rq071", new String[] { "RQ071Input", "RQ071Output" });
    }

    /**
     * Generate binding for Redsimpt.
     * 
     * @throws Exception if generation fails
     */
    public void testGenRedsimpt() throws Exception {
        genSourceAndCheck("redsimpt", "Dfhcommarea");
        String custSrce = getSource(getGetCustFilename("redsimpt"));
        assertTrue(custSrce
                .contains("Dfhcommarea valueObject = (Dfhcommarea) choice.getParentValueObject();"));
    }

    /**
     * Generates COXB classes and chack against reference.
     * 
     * @param schemaFileName the originating XSD file name
     * @param rootName JAXB root class name
     * @throws Exception usually if test not set correctly
     */
    protected void genSourceAndCheck(final File schemaFileName,
            final String rootName) throws Exception {
        String schemaName = FilenameUtils.getBaseName(schemaFileName.getName())
                .toLowerCase();
        genSourceAndCheck(schemaName, new String[] { rootName });
    }

    /**
     * Generates COXB classes and chack against reference.
     * 
     * @param schemaName the originating XSD name
     * @param rootName JAXB root class name
     * @throws Exception usually if test not set correctly
     */
    protected void genSourceAndCheck(final String schemaName,
            final String rootName) throws Exception {
        genSourceAndCheck(schemaName, new String[] { rootName });
    }

    /**
     * Generates COXB classes and chack against reference.
     * 
     * @param schemaFileName the originating XSD file name
     * @param rootNames JAXB root class names
     * @throws Exception usually if test not set correctly
     */
    protected void genSourceAndCheck(final File schemaFileName,
            final String[] rootNames) throws Exception {
        String schemaName = FilenameUtils.getBaseName(schemaFileName.getName())
                .toLowerCase();
        genSourceAndCheck(schemaName, rootNames);
    }

    /**
     * Generates COXB classes and chack against reference.
     * 
     * @param schemaName the originating XSD name
     * @param rootNames JAXB root class names
     * @throws Exception usually if test not set correctly
     */
    protected void genSourceAndCheck(final String schemaName,
            final String[] rootNames) throws Exception {
        genSource(schemaName, rootNames, true, true);
        check(schemaName.replace(".", "/"));
    }

    /**
     * Generates COXB classes.
     * 
     * @param schemaName the originating XSD name
     * @param rootNames JAXB root class names
     * @param xmlTransformers true to generate XML transformers
     * @param jsonTransformers true to generate JSON transformers
     */
    protected void genSource(final String schemaName, final String[] rootNames,
            final boolean xmlTransformers, final boolean jsonTransformers) {
        CoxbBindingGenerator gen = new CoxbBindingGenerator();
        gen.setJaxbBinDir(JAXB_BIN_DIR);
        gen.setJaxbPackageName(JAXB_PKG_PFX + '.' + schemaName);
        for (String rootName : rootNames) {
            gen.addJaxbRootClass(rootName);
        }
        gen.setTargetDir(GEN_SRC_DIR);
        gen.setXmlTransformers(xmlTransformers);
        gen.setJsonTransformers(jsonTransformers);
        gen.execute();
    }
}
