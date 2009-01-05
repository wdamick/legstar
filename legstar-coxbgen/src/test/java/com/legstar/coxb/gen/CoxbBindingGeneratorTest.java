/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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


/**
 * Test the binding generator.
 *
 */
public class CoxbBindingGeneratorTest extends AbstractTestTemplate {

    /** Sub directory for binding classes. */
    private static final String GEN_SRC_SUBDIR = "com/legstar/test/coxb";

    /** Sub directory for custom classes. */
    private static final String GEN_CUST_SUBDIR = "com/legstar/coxb/cust";

    /** Generated JAXB classes package prefix. */
    private static final String JAXB_PKG_PFX = "com.legstar.test.coxb";


    /** Make sure we have a clean output folder. */
    public void setUp() {
        java.io.File td = GEN_SRC_DIR;
        td.delete();
        td.mkdirs();
    }

    /** Generator should check on package name. */
    public void testCheckOnPackageName() {
        try {
            CoxbBindingGenerator gen = new CoxbBindingGenerator();
            gen.execute();
        } catch (RuntimeException e) {
            assertEquals("You must specify either a JAXB package name or an XML schema file name", e.getMessage());
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
            assertEquals("You must specify at least one JAXB root class name", e.getMessage());
        }
    }

    /**
     * Generate binding for Alltypes.
     * @throws Exception if generation fails
     */
   public void testGenAlltypes() throws Exception  {
        genSource("alltypes", "Dfhcommarea");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("alltypes", "Dfhcommarea"));
        assertTrue(srce.contains("sString = BF.createStringBinding(\"SString\","));
        assertTrue(srce.contains("\"SString\", String.class, this);"));
    }

    /**
     * Generate binding for Dplarcht.
     * @throws Exception if generation fails
     */
    public void testGenDplarcht() throws Exception   {
        genSource("dplarcht", "Dfhcommarea");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("dplarcht", "Dfhcommarea"));
        assertTrue(srce.contains("lsRequest = new LsRequestBinding(\"LsRequest\","));
        assertTrue(srce.contains("\"LsRequest\", this, null);"));
        assertTrue(srce.contains("lsReply = new LsReplyBinding(\"LsReply\","));
        assertTrue(srce.contains("\"LsReply\", this, null);"));
    }

    /**
     * Generate binding for Redsimpt.
     * @throws Exception if generation fails
     */
    public void testGenRedsimpt() throws Exception   {
        File custFile = new File(getGetCustFilename("redsimpt"));
        custFile.delete();
        genSource("redsimpt", "Dfhcommarea");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("redsimpt", "Dfhcommarea"));
        assertTrue(srce.contains("cDefinition1Choice = new CDefinition1ChoiceBinding(\"CDefinition1Choice\", this);"));
        assertTrue(srce.contains("cDefinition1Choice.setUnmarshalChoiceStrategyClassName("));
        assertTrue(srce.contains("\"com.legstar.coxb.cust.redsimpt.ChoiceSelector\");"));
        String custSrce = getSource(GEN_SRC_DIR, getGetCustFilename("redsimpt"));
        assertTrue(custSrce.contains(
                "Dfhcommarea valueObject = (Dfhcommarea) choice.getObjectValue(Dfhcommarea.class);"));
    }

    /**
     * Generate binding for Arrayssm.
     * @throws Exception if generation fails
     */
    public void testGenArrayssm() throws Exception   {
        genSource("arrayssm", "Dfhcommarea");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("arrayssm", "Dfhcommarea"));
        assertTrue(srce.contains("tableComplexWrapperItem = new TableComplexBinding(\"TableComplexWrapperItem\","));
        assertTrue(srce.contains("\"TableComplex\", this, null);"));
        assertTrue(srce.contains("tableComplexWrapper = new TableComplexWrapperBinding(\"TableComplexWrapper\","));
        assertTrue(srce.contains("\"TableComplex\", this, _tableComplexWrapperItem);"));
    }

    /**
     * Generate binding for Lsfileae.
     * @throws Exception if generation fails
     */
    public void testGenLsfileae() throws Exception   {
        genSource("lsfileae", "Dfhcommarea");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("lsfileae", "Dfhcommarea"));
        assertTrue(srce.contains("comNumber = BF.createZonedDecimalBinding(\"ComNumber\","));
        assertTrue(srce.contains("\"ComNumber\", Long.class, this);"));
        srce = getSource(GEN_SRC_DIR, getSrcFilename("lsfileae", "DfhcommareaHostToJavaTransformer"));
        assertTrue(srce.contains(
                "public class DfhcommareaHostToJavaTransformer extends AbstractHostToJavaTransformer {"));
        srce = getSource(GEN_SRC_DIR, getSrcFilename("lsfileae", "DfhcommareaJavaToHostTransformer"));
        assertTrue(srce.contains(
                "public class DfhcommareaJavaToHostTransformer extends AbstractJavaToHostTransformer {"));
        srce = getSource(GEN_SRC_DIR, getSrcFilename("lsfileae", "DfhcommareaTransformerProvider"));
        assertTrue(srce.contains(
                "public class DfhcommareaTransformerProvider extends AbstractTransformerProvider {"));
    }

    /**
     * Generate binding for Arraysdo.
     * @throws Exception if generation fails
     */
    public void testGenArraysdo() throws Exception   {
        genSource("arraysdo", "Dfhcommarea");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("arraysdo", "Dfhcommarea"));
        assertTrue(srce.contains("tableOdo = BF.createArrayStringBinding(\"TableOdo\","));
        assertTrue(srce.contains("\"TableOdo\", String.class, this);"));
        assertTrue(srce.contains("tableOdo.setByteLength(5);"));
        assertTrue(srce.contains("tableOdo.setCobolName(\"TABLE-ODO\");"));
        assertTrue(srce.contains("tableOdo.setMinOccurs(1);"));
        assertTrue(srce.contains("tableOdo.setMaxOccurs(100);"));
        assertTrue(srce.contains("tableOdo.setDependingOn(\"TABLE-SIZE\");"));
    }

    /**
     * Generate binding for Listssdo.
     * @throws Exception if generation fails
     */
    public void testGenListssdo() throws Exception   {
        genSource("listssdo", "Dfhcommarea");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("listssdo", "Dfhcommarea"));
        assertTrue(srce.contains("listOdoCounter = BF.createBinaryBinding(\"ListOdoCounter\","));
        assertTrue(srce.contains("this);"));
        assertTrue(srce.contains("listOdoCounter.setByteLength(4);"));
        assertTrue(srce.contains("listOdoCounter.setCobolName(\"LIST-ODO--C\");"));
        assertTrue(srce.contains("listOdoCounter.setTotalDigits(9);"));
        assertTrue(srce.contains("listOdoCounter.setIsODOObject(true);"));
        assertTrue(srce.contains("listOdo = BF.createArrayStringBinding(\"ListOdo\","));
        assertTrue(srce.contains("\"ListOdo\", String.class, this);"));
    }

    /**
     * Generate binding for MSNSearch SearchRequestType.
     * @throws Exception if generation fails
     */
    public void testMSNSearchSearchRequestType() throws Exception   {
        genSource("MSNSearch", "SearchRequestType");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("MSNSearch", "SearchRequestType"));
        assertTrue(srce.contains("flagsCounter = BF.createBinaryBinding(\"FlagsCounter\","));
        assertTrue(srce.contains("this);"));
        assertTrue(srce.contains("flagsCounter.setByteLength(4);"));
        assertTrue(srce.contains("flagsCounter.setCobolName(\"Flags--C\");"));
        assertTrue(srce.contains("flagsCounter.setTotalDigits(9);"));
        assertTrue(srce.contains("flagsCounter.setIsODOObject(true);"));
        assertTrue(srce.contains("safeSearch = BF.createStringBinding(\"SafeSearch\","));
        assertTrue(srce.contains("\"SafeSearch\", SafeSearchOptionsType.class, this);"));
    }

    /**
     * Generate binding for MSNSearch SearchResponseType.
     * @throws Exception if generation fails
     */
    public void testMSNSearchSearchResponseType() throws Exception   {
        genSource("MSNSearch", "SearchResponse");
        String srce = getSource(GEN_SRC_DIR, getBindingSrcFilename("MSNSearch", "SourceResponseType"));
        assertTrue(srce.contains("source = BF.createStringBinding(\"Source\","));
        assertTrue(srce.contains("\"Source\", SourceTypeType.class, this);"));
        assertTrue(srce.contains("source.setByteLength(32);"));
        assertTrue(srce.contains("source.setCobolName(\"R-Source\");"));
        assertTrue(srce.contains("results = new ArrayOfResultResultsTypeBinding(\"Results\","));
        assertTrue(srce.contains("\"Results\", this, null);"));
    }

    /**
     * Check that we can provide multiple class names at once.
     * @throws Exception if generation fails
     */
    public void testMultipleJaxbRootClassNames() throws Exception {
        CoxbBindingGenerator gen = new CoxbBindingGenerator();
        gen.setJaxbBinDir(JAXB_BIN_DIR);
        gen.setJaxbPackageName(JAXB_PKG_PFX + '.' + "cultureinfo");
        gen.addJaxbRootClass("CultureInfoParameters");
        gen.addJaxbRootClass("CultureInfoReply");
        gen.setTargetDir(GEN_SRC_DIR);
        gen.execute();
        String src1 = getSource(GEN_SRC_DIR, getBindingSrcFilename("cultureinfo", "CultureInfoParameters"));
        assertTrue(src1.contains("cultureCode = BF.createStringBinding(\"CultureCode\","));
        String src2 = getSource(GEN_SRC_DIR, getBindingSrcFilename("cultureinfo", "CultureInfoReply"));
        assertTrue(src2.contains("currencySymbol = BF.createStringBinding(\"CurrencySymbol\","));
    }

    /**
     * Generates COXB classes.
     * @param schemaName the originating XSD name
     * @param rootName JAXB root class name
     */
    private void genSource(final String schemaName, final String rootName) {
        CoxbBindingGenerator gen = new CoxbBindingGenerator();
        gen.setJaxbBinDir(JAXB_BIN_DIR);
        gen.setJaxbPackageName(JAXB_PKG_PFX + '.' + schemaName);
        gen.setJaxbRootClassName(rootName);
        gen.setTargetDir(GEN_SRC_DIR);
        gen.execute();
    }

    /**
     * @param schemaName the originating XSD name
     * @param className the bound class name
     * @return the qualified Binding class file name
     */
    private String getBindingSrcFilename(final String schemaName, final String className) {
        return GEN_SRC_SUBDIR + '/' + schemaName + "/bind/" + className + "Binding.java";
    }

    /**
     * @param schemaName the originating XSD name
     * @param className the bound class name
     * @return the qualified class file name
     */
    private String getSrcFilename(final String schemaName, final String className) {
        return GEN_SRC_SUBDIR + '/' + schemaName + "/bind/" + className + ".java";
    }

    /**
     * @param schemaName the originating XSD name
     * @return the qualified custom class file name
     */
    private String getGetCustFilename(final String schemaName) {
        return GEN_CUST_SUBDIR + '/' + schemaName + "/ChoiceSelector.java";
    }

}
