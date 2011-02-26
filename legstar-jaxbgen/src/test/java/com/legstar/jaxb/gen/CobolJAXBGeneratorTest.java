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
package com.legstar.jaxb.gen;

import java.io.File;

import org.apache.commons.io.FileUtils;
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
        _task.setXsdFile(getSchemaFromFolder("lsfileaq"));
        try {
            _task.execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must specify a destination directory",
                    e.getMessage());
        }
    }

    /**
     * Test without any extra parameters.
     * 
     * @throws Exception if generation fails
     */
    public void testDefaultGeneration() throws Exception {
        _task.setXsdFile(getSchemaFromFolder("lsfileaq"));
        _task.setTargetDir(GEN_SRC_DIR);
        _task.execute();
        String srce = getJaxbSource("lsfileaq", "DfhCommarea");
        assertTrue(srce.contains("@CobolElement(cobolName = \"QUERY-DATA\","
                + " type = CobolType.GROUP_ITEM," + " levelNumber = 5,"
                + " srceLine = 36)"));
    }

    /**
     * Test with package name parameters.
     * 
     * @throws Exception if generation fails
     */
    public void testPackageName() throws Exception {
        _task.setXsdFile(getSchemaFromFolder("lsfileaq"));
        _task.setTargetDir(GEN_SRC_DIR);
        _task.setJaxbPackageName("com.alternate.pkg.lsfileaq");
        _task.execute();
        String srce = getSource("lsfileaq", "com/alternate/pkg", "DfhCommarea");
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
        globalBindings(internalBindings, "lsfileaq", 1L, true);
        assertTrue(getJaxbSource("lsfileaq", "DfhCommarea").contains(
                "public boolean isSetReplyData()"));

        globalBindings(internalBindings, "lsfileaq", 1L, false);
        assertFalse(getJaxbSource("lsfileaq", "DfhCommarea").contains(
                "public boolean isSetReplyData()"));

        globalBindings(internalBindings, "lsfileaq", 1L, true);
        assertTrue(getJaxbSource("lsfileaq", "DfhCommarea").contains(
                "private final static long serialVersionUID = 1L;"));

        globalBindings(internalBindings, "lsfileaq", 123589357872112454L, true);
        assertTrue(getJaxbSource("lsfileaq", "DfhCommarea")
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
        nameTransform(internalBindings, "lsfileaq", "SomePrefix", null, null,
                null);
        assertTrue(getJaxbSource("lsfileaq", "SomePrefixDfhCommarea").contains(
                "public class SomePrefixDfhcommarea"));

        nameTransform(internalBindings, "lsfileaq", null, "SomeSuffix", null,
                null);
        assertTrue(getJaxbSource("lsfileaq", "DfhCommareaSomeSuffix").contains(
                "public class DfhcommareaSomeSuffix"));

        nameTransform(internalBindings, "lsfileaq", "SomePrefix", "SomeSuffix",
                null, null);
        assertTrue(getJaxbSource("lsfileaq", "SomePrefixDfhCommareaSomeSuffix")
                .contains("public class SomePrefixDfhcommareaSomeSuffix"));

        nameTransform(internalBindings, "MSNSearch", null, null, "SomePrefix",
                null);
        assertTrue(getJaxbSource("MSNSearch", "SomePrefixSearchResponse").contains(
                "public class SomePrefixSearchResponse"));

        nameTransform(internalBindings, "MSNSearch", null, null, null,
                "SomeSuffix");
        assertTrue(getJaxbSource("MSNSearch", "SearchResponseSomeSuffix").contains(
                "public class SearchResponseSomeSuffix"));

        nameTransform(internalBindings, "MSNSearch", null, null, "SomePrefix",
                "SomeSuffix");
        assertTrue(getJaxbSource("MSNSearch", "SomePrefixSearchResponseSomeSuffix")
                .contains("public class SomePrefixSearchResponseSomeSuffix"));

    }

    /**
     * A helper method for global bindings tests.
     * 
     * @param internalBindings uses internal or external bindings
     * @param schemaName the schema name
     * @param serializableUid the serial unique ID
     * @param generateIsSetMethod if generate is set methods
     * @throws Exception if generation fails
     */
    protected void globalBindings(final boolean internalBindings,
            final String schemaName, final long serializableUid,
            final boolean generateIsSetMethod) throws Exception {
        _task.setInternalBindings(internalBindings);
        _task.setXsdFile(getSchemaFromFolder(schemaName));
        _task.setTargetDir(GEN_SRC_DIR);
        _task.setSerializableUid(serializableUid);
        _task.setGenerateIsSetMethod(generateIsSetMethod);
        _task.setJaxbPackageName("com.legstar.test.coxb." + schemaName);
        _task.execute();
    }

    /**
     * A helper method for name transformation tests.
     * 
     * @param internalBindings uses internal or external bindings
     * @param schemaName the schema name
     * @param typeNamePrefix type name prefix
     * @param typeNameSuffix type name suffix
     * @param elementNamePrefix element name prefix
     * @param elementNameSuffix element name suffix
     * @throws Exception if generation fails
     */
    protected void nameTransform(final boolean internalBindings,
            final String schemaName, final String typeNamePrefix,
            final String typeNameSuffix, final String elementNamePrefix,
            final String elementNameSuffix) throws Exception {
        _task.setInternalBindings(internalBindings);
        _task.setXsdFile(getSchemaFromFolder(schemaName));
        _task.setTargetDir(GEN_SRC_DIR);
        _task.setTypeNamePrefix(typeNamePrefix);
        _task.setTypeNameSuffix(typeNameSuffix);
        _task.setElementNamePrefix(elementNamePrefix);
        _task.setElementNameSuffix(elementNameSuffix);
        _task.setJaxbPackageName("com.legstar.test.coxb." + schemaName);
        _task.execute();
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

        _task.setXsdFile(tempXsdFile);
        _task.setTargetDir(GEN_SRC_DIR);
        _task.execute();

        String result = getSource("generated", "", "CustomerType");
        assertTrue(result.contains("public String getNAme()"));

    }

    /**
     * An ECI compatibility case.
     */
    public void testRQ071() throws Exception {
        _task.setXsdFile(new File(COB_XSD_DIR, "RQ071CICSECIBinding.xsd"));
        _task.setEciCompatible(true);
        _task.setJaxbPackageName("com.legstar.test.coxb.rq071");
        _task.setTargetDir(GEN_SRC_DIR);
        _task.execute();
        check(new File(SRC_REF_DIR, "com/legstar/test/coxb/rq071"), new File(
                GEN_SRC_DIR, "com/legstar/test/coxb/rq071"), "java");
    }
}
