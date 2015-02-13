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
package com.legstar.jaxb.gen;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;

import com.legstar.jaxb.AbstractJaxbGenTest;

/**
 * Test cases for the JAXB generator.
 * 
 */
public class CobolJAXBGeneratorTest extends AbstractJaxbGenTest {

    /** An instance of the JAXB generator. */
    private CobolJAXBGenerator _task;

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /** List of XSDs which need special generation parameters. */
    private static final List < String > NON_STANDARD_XSDS = Arrays
            .asList(new String[] { "enumvar.xsd", "MSNSearch.xsd",
                    "jvmquery.xsd", "jvmquery-ws.xsd", "cultureinfo.xsd",
                    "RQ071CICSECIBinding.xsd" });

    /** {@inheritDoc} */
    @Override
    public void setUp() throws Exception {
        super.setUp();
        _task = new CobolJAXBGenerator();
        _task.setProject(new Project());
        _task.init();
        _task.getProject().fireBuildStarted();
        setCreateReferences(CREATE_REFERENCES);

    }

    /**
     * Test the input checking.
     * 
     * @throws Exception if generation fails
     */
    public void testCheckInput() throws Exception {
        try {
            _task.execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must specify an XML schema file name",
                    e.getMessage());
        }
        _task.setXsdFile(new File(COB_XSD_DIR, "LSFILEAQ.xsd"));
        try {
            _task.execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must specify a destination directory",
                    e.getMessage());
        }
    }

    /**
     * Test with package name parameters.
     * 
     * @throws Exception if generation fails
     */
    public void testPackageName() throws Exception {
        _task.setXsdFile(new File(COB_XSD_DIR, "LSFILEAQ.xsd"));
        _task.setTargetDir(GEN_SRC_DIR);
        _task.setJaxbPackageName("com.alternate.pkg.lsfileaq");
        _task.execute();
        String srce = getSource("lsfileaq", "com/alternate/pkg", "Dfhcommarea");
        assertTrue(srce.contains("@CobolElement(cobolName = \"QUERY-DATA\","
                + " type = CobolType.GROUP_ITEM," + " levelNumber = 5,"
                + " srceLine = 36)"));
    }

    /**
     * Test global bindings with external and internal bindings.
     * 
     * @throws Exception if generation fails
     */
    public void testGlobalBindings() throws Exception {
        globalBindings(false);
        globalBindings(true);

    }

    /**
     * Test name transform with external and internal bindings.
     * 
     * @throws Exception if generation fails
     */
    public void testNameTransform() throws Exception {
        nameTransform(false);
        nameTransform(true);
    }

    /**
     * Test global bindings with external bindings.
     * 
     * @param internalBindings uses internal or external bindings
     * @throws Exception if generation fails
     */
    protected void globalBindings(final boolean internalBindings)
            throws Exception {
        jaxbgen("LSFILEAQ.xsd", "lsfileaq", internalBindings, 1L, true, null,
                null, null, null, false, false);
        assertTrue(getJaxbSource("lsfileaq", "Dfhcommarea").contains(
                "public boolean isSetReplyData()"));

        jaxbgen("LSFILEAQ.xsd", "lsfileaq", internalBindings, 1L, false, null,
                null, null, null, false, false);
        assertFalse(getJaxbSource("lsfileaq", "Dfhcommarea").contains(
                "public boolean isSetReplyData()"));

        jaxbgen("LSFILEAQ.xsd", "lsfileaq", internalBindings, 1L, true, null,
                null, null, null, false, false);
        assertTrue(getJaxbSource("lsfileaq", "Dfhcommarea").contains(
                "private final static long serialVersionUID = 1L;"));

        jaxbgen("LSFILEAQ.xsd", "lsfileaq", internalBindings,
                123589357872112454L, true, null, null, null, null, false, false);
        assertTrue(getJaxbSource("lsfileaq", "Dfhcommarea")
                .contains(
                        "private final static long serialVersionUID = 123589357872112454L;"));
    }

    /**
     * Test name transform.
     * 
     * @param internalBindings uses internal or external bindings
     * @throws Exception if generation fails
     */
    public void nameTransform(final boolean internalBindings) throws Exception {
        jaxbgen("LSFILEAQ.xsd", "lsfileaq", internalBindings, 1L, true,
                "SomePrefix", null, null, null, false, false);
        assertTrue(getJaxbSource("lsfileaq", "SomePrefixDfhcommarea").contains(
                "public class SomePrefixDfhcommarea"));

        jaxbgen("LSFILEAQ.xsd", "lsfileaq", internalBindings, 1L, true, null,
                "SomeSuffix", null, null, false, false);
        assertTrue(getJaxbSource("lsfileaq", "DfhcommareaSomeSuffix").contains(
                "public class DfhcommareaSomeSuffix"));

        jaxbgen("LSFILEAQ.xsd", "lsfileaq", internalBindings, 1L, true,
                "SomePrefix", "SomeSuffix", null, null, false, false);
        assertTrue(getJaxbSource("lsfileaq", "SomePrefixDfhcommareaSomeSuffix")
                .contains("public class SomePrefixDfhcommareaSomeSuffix"));

        jaxbgen("MSNSearch.xsd", "MSNSearch", internalBindings, 1L, true, null,
                null, "SomePrefix", null, false, false);
        assertTrue(getJaxbSource("MSNSearch", "SomePrefixSearchResponse")
                .contains("public class SomePrefixSearchResponse"));

        jaxbgen("MSNSearch.xsd", "MSNSearch", internalBindings, 1L, true, null,
                null, null, "SomeSuffix", false, false);
        assertTrue(getJaxbSource("MSNSearch", "SearchResponseSomeSuffix")
                .contains("public class SearchResponseSomeSuffix"));

        jaxbgen("MSNSearch.xsd", "MSNSearch", internalBindings, 1L, true, null,
                null, "SomePrefix", "SomeSuffix", false, false);
        assertTrue(getJaxbSource("MSNSearch",
                "SomePrefixSearchResponseSomeSuffix").contains(
                "public class SomePrefixSearchResponseSomeSuffix"));

    }

    /**
     * Check what JAXB does with underscores.
     */
    public void testElementNamesWithUnderscores() throws Exception {
        String xsd = "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\">"
                + "<xs:element name=\"customer\" type=\"CustomerType\"/>"
                + "<xs:complexType name=\"CustomerType\">" + "  <xs:sequence>"
                + "    <xs:element name=\"n_ame\" type=\"xs:string\"/>"
                + "    <xs:element name=\"number\" type=\"xs:integer\"/>"
                + "  </xs:sequence>" + "</xs:complexType>" + "</xs:schema>";
        File tempXsdFile = File.createTempFile("jaxbgen", ".xsd");
        tempXsdFile.deleteOnExit();
        FileUtils.writeStringToFile(tempXsdFile, xsd);

        jaxbgen("customer", tempXsdFile, true, 1L, true, null, null, null,
                null, false, false);
        checkLocalRef("customer", "CustomerType.java");
    }

    /**
     * Check the no packae-info option.
     */
    public void testNoPackageInfo() throws Exception {
        String xsd = "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\""
                + " xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\">"
                + "<xs:element name=\"customer\" type=\"CustomerType\"/>"
                + "<xs:complexType name=\"CustomerType\">" + "  <xs:sequence>"
                + "    <xs:element name=\"name\" type=\"xs:string\"/>"
                + "    <xs:element name=\"number\" type=\"xs:integer\"/>"
                + "  </xs:sequence>" + "</xs:complexType>" + "</xs:schema>";
        File tempXsdFile = File.createTempFile("jaxbgen", ".xsd");
        tempXsdFile.deleteOnExit();
        FileUtils.writeStringToFile(tempXsdFile, xsd);

        jaxbgen("customer", tempXsdFile, true, 1L, true, null, null, null,
                null, false, true);
        assertTrue(new File(GEN_SRC_DIR,
                "com/legstar/test/coxb/customer/CustomerType.java").exists());
        assertFalse(new File(GEN_SRC_DIR,
                "com/legstar/test/coxb/customer/package-info.java").exists());
    }

    /**
     * Test all standard cases.
     * 
     * @throws Exception if generation fails
     */
    public void testAllStandard() throws Exception {
        Collection < File > xsdFiles = FileUtils.listFiles(COB_XSD_DIR,
                new String[] { "xsd" }, false);
        for (File schemaFile : xsdFiles) {
            if (!NON_STANDARD_XSDS.contains(schemaFile.getName())) {
                jaxbgenAndCheck(schemaFile);
            }
        }
    }

    /**
     * An enum.
     */
    public void testEnumvar() throws Exception {
        jaxbgen("enumvar", new File(COB_XSD_DIR, "enumvar.xsd"), true, 1L,
                true, null, "Type", null, null, false, false);
        check("enumvar");
    }

    /**
     * The MSNSearch case.
     */
    public void testMSNSearch() throws Exception {
        jaxbgen("MSNSearch", new File(COB_XSD_DIR, "MSNSearch.xsd"), true, 1L,
                true, null, "Type", null, null, false, false);
        check("MSNSearch");
    }

    /**
     * The JVMQuery case.
     */
    public void testJvmquery() throws Exception {
        jaxbgen("jvmquery", new File(COB_XSD_DIR, "jvmquery.xsd"), true, 1L,
                true, null, null, null, null, false, false);
        check("jvmquery");
    }

    /**
     * The JVMQuery Web Service case.
     */
    public void testJvmqueryWs() throws Exception {
        jaxbgen("ws.jvmquery", new File(COB_XSD_DIR, "jvmquery-ws.xsd"), true,
                1L, true, null, null, null, null, false, false);
        check("ws.jvmquery");
    }

    /**
     * The CultureInfo Web Service case.
     */
    public void testCultureinfo() throws Exception {
        jaxbgen("cultureinfo", new File(COB_XSD_DIR, "cultureinfo.xsd"), true,
                1L, true, null, null, null, null, false, false);
        check("cultureinfo");
    }

    /**
     * An ECI compatibility case.
     */
    public void testRQ071() throws Exception {
        jaxbgen("rq071", new File(COB_XSD_DIR, "RQ071CICSECIBinding.xsd"),
                true, 1L, true, null, null, null, null, true, false);
        check("rq071");
    }

    /**
     * Level 88 case.
     */
    public void testRQ074() throws Exception {
        jaxbgen("rq074", new File(COB_XSD_DIR, "rq074.xsd"), true, 1L, true,
                null, null, null, null, false, false);
        check("rq074");
    }

    /**
     * A helper method to check generated code against reference.
     * 
     * @param schemaFile the schema file
     * @throws Exception if generation fails
     */
    protected void jaxbgenAndCheck(final File schemaFile) throws Exception {
        String schemaName = FilenameUtils.getBaseName(schemaFile.getName())
                .toLowerCase();
        jaxbgen(schemaName, schemaFile, true, 1L, true, null, null, null, null,
                false, false);
        check(schemaName);
    }

    /**
     * A helper method to generate and check against a reference.
     * 
     * @param schemaName the schema name
     * @throws Exception if generation fails
     */
    protected void check(final String schemaName) throws Exception {
        check(new File(SRC_REF_DIR, GEN_SRC_SUBDIR + "/"
                + schemaName.replace(".", "/")), new File(GEN_SRC_DIR,
                GEN_SRC_SUBDIR + "/" + schemaName.replace(".", "/")), "java");
    }

    /**
     * A helper method for JAXB generation.
     * 
     * @param schemaFileName the schema file name
     * @param jaxbPackageSuffix the name used to create a unique package name
     *            for the generated JAXB classes
     * @param internalBindings uses internal or external bindings
     * @param serializableUid the serial unique ID
     * @param generateIsSetMethod if generate is set methods
     * @param typeNamePrefix type name prefix
     * @param typeNameSuffix type name suffix
     * @param elementNamePrefix element name prefix
     * @param elementNameSuffix element name suffix
     * @param eciCompatible if generated JAXB class should be ECI compatible
     * @param noPackageInfo if package-info should not be generated
     * @throws Exception if generation fails
     */
    protected void jaxbgen(final String schemaFileName,
            final String jaxbPackageSuffix, final boolean internalBindings,
            final long serializableUid, final boolean generateIsSetMethod,
            final String typeNamePrefix, final String typeNameSuffix,
            final String elementNamePrefix, final String elementNameSuffix,
            final boolean eciCompatible, final boolean noPackageInfo)
            throws Exception {
        jaxbgen(jaxbPackageSuffix, new File(COB_XSD_DIR, schemaFileName),
                internalBindings, serializableUid, generateIsSetMethod,
                typeNamePrefix, typeNameSuffix, elementNamePrefix,
                elementNameSuffix, eciCompatible, noPackageInfo);
    }

    /**
     * A helper method for JAXB generation.
     * 
     * @param jaxbPackageSuffix the name used to create a unique package name
     *            for the generated JAXB classes
     * @param xsdFile the XML schema file
     * @param internalBindings uses internal or external bindings
     * @param serializableUid the serial unique ID
     * @param generateIsSetMethod if generate is set methods
     * @param typeNamePrefix type name prefix
     * @param typeNameSuffix type name suffix
     * @param elementNamePrefix element name prefix
     * @param elementNameSuffix element name suffix
     * @param eciCompatible if generated JAXB class should be ECI compatible
     * @param noPackageInfo if package-info should not be generated
     * @throws Exception if generation fails
     */
    protected void jaxbgen(final String jaxbPackageSuffix, final File xsdFile,
            final boolean internalBindings, final long serializableUid,
            final boolean generateIsSetMethod, final String typeNamePrefix,
            final String typeNameSuffix, final String elementNamePrefix,
            final String elementNameSuffix, final boolean eciCompatible,
            final boolean noPackageInfo) throws Exception {
        _task.setInternalBindings(internalBindings);
        _task.setXsdFile(xsdFile);
        _task.setJaxbPackageName(JAXB_PKG_PFX + "." + jaxbPackageSuffix);
        _task.setTargetDir(GEN_SRC_DIR);
        _task.setSerializableUid(serializableUid);
        _task.setGenerateIsSetMethod(generateIsSetMethod);
        _task.setTypeNamePrefix(typeNamePrefix);
        _task.setTypeNameSuffix(typeNameSuffix);
        _task.setElementNamePrefix(elementNamePrefix);
        _task.setElementNameSuffix(elementNameSuffix);
        _task.setEciCompatible(eciCompatible);
        _task.setNoPackageInfo(noPackageInfo);
        _task.execute();
    }

}
