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

/**
 * Test the binding generator.
 * 
 */
public class CoxbBindingGeneratorTest extends AbstractCoxbGenTest {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

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
     * Generate binding for Alltypes.
     * 
     * @throws Exception if generation fails
     */
    public void testGenAlltypes() throws Exception {
        genSourceAndCheck("alltypes", "Dfhcommarea");
    }

    /**
     * Generate binding for Arraysdo.
     * 
     * @throws Exception if generation fails
     */
    public void testGenArraysdo() throws Exception {
        genSourceAndCheck("arraysdo", "Dfhcommarea");
    }

    /**
     * Generate binding for Arrayssm.
     * 
     * @throws Exception if generation fails
     */
    public void testGenArrayssm() throws Exception {
        genSourceAndCheck("arrayssm", "Dfhcommarea");
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
     * Generate binding for Dplarcht.
     * 
     * @throws Exception if generation fails
     */
    public void testGenDplarcht() throws Exception {
        genSourceAndCheck("dplarcht", "Dfhcommarea");
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
     * Generate binding for JvmQuery.
     * 
     * @throws Exception if generation fails
     */
    public void testJvmQuery() throws Exception {
        genSourceAndCheck("jvmquery", new String[] { "JvmQueryRequest",
                "JvmQueryReply" });
    }

    /**
     * Generate binding for Lsfileae (with XML and JSON Transformers).
     * 
     * @throws Exception if generation fails
     */
    public void testGenLsfileae() throws Exception {
        genSourceAndCheck("lsfileae", "Dfhcommarea");
    }

    /**
     * Generate binding for Listssdo.
     * 
     * @throws Exception if generation fails
     */
    public void testGenListssdo() throws Exception {
        genSourceAndCheck("listssdo", "Dfhcommarea");
    }

    /**
     * Generate binding for MSNSearch.
     * 
     * @throws Exception if generation fails
     */
    public void testMSNSearch() throws Exception {
        genSourceAndCheck("MSNSearch", new String[] { "SearchRequestType",
                "SearchResponse" });
    }

    public void testRq071() throws Exception {
        genSourceAndCheck("rq071", new String[] { "RQ071Input", "RQ071Output" });
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
     * @param schemaName the originating XSD name
     * @param rootName JAXB root class name
     * @throws Exception usually if test not set correctly
     */
    private void genSourceAndCheck(final String schemaName,
            final String rootName) throws Exception {
        genSourceAndCheck(schemaName, new String[] { rootName });
    }

    /**
     * Generates COXB classes and chack against reference.
     * 
     * @param schemaName the originating XSD name
     * @param rootName JAXB root class name
     * @throws Exception usually if test not set correctly
     */
    private void genSourceAndCheck(final String schemaName,
            final String[] rootNames) throws Exception {
        genSource(schemaName, rootNames, true, true);
        check(schemaName);
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
