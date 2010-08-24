package com.legstar.jaxb.gen;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;

import com.legstar.jaxb.AbstractJaxbTester;

/**
 * Test cases for the JAXB generator.
 * 
 */
public class CobolJAXBGeneratorTest extends AbstractJaxbTester {

    /** An instance of the JAXB generator. */
    private CobolJAXBGenerator _task;

    /** {@inheritDoc} */
    @Override
    public void setUp() throws Exception {
        super.setUp();
        _task = new CobolJAXBGenerator();
        _task.setProject(new Project());
        _task.init();
        _task.getProject().fireBuildStarted();

    }

    /**
     * Test the input checking.
     */
    public void testCheckInput() {
        try {
            _task.execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must specify an XML schema file name", e
                    .getMessage());
        }
        _task.setXsdFile(getSchemaFromResources("lsfileaq"));
        try {
            _task.execute();
            fail();
        } catch (BuildException e) {
            assertEquals("You must specify a destination directory", e
                    .getMessage());
        }
    }

    /**
     * Test without any extra parameters.
     */
    public void testDefaultGeneration() {
        _task.setXsdFile(getSchemaFromResources("lsfileaq"));
        _task.setTargetDir(GEN_SRC_DIR);
        _task.execute();
        String srce = getSource("lsfileaq", "DfhCommarea");
        assertTrue(srce.contains("@CobolElement(cobolName = \"QUERY-DATA\","
                + " type = CobolType.GROUP_ITEM," + " levelNumber = 5,"
                + " srceLine = 36)"));
    }

    /**
     * Test with package name parameters.
     */
    public void testPackageName() {
        _task.setXsdFile(getSchemaFromResources("lsfileaq"));
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
     */
    public void testGlobalBindings() {
        globalBindings(false);
        globalBindings(true);

    }

    /**
     * Test name transform with external and internal bindings.
     */
    public void testNameTransform() {
        nameTransform(false);
        nameTransform(true);
    }

    /**
     * Test global bindings with external bindings.
     * 
     * @param internalBindings uses internal or external bindings
     */
    protected void globalBindings(final boolean internalBindings) {
        globalBindings(internalBindings, "lsfileaq", 1L, true);
        assertTrue(getSource("lsfileaq", "DfhCommarea").contains(
                "public boolean isSetReplyData()"));

        globalBindings(internalBindings, "lsfileaq", 1L, false);
        assertFalse(getSource("lsfileaq", "DfhCommarea").contains(
                "public boolean isSetReplyData()"));

        globalBindings(internalBindings, "lsfileaq", 1L, true);
        assertTrue(getSource("lsfileaq", "DfhCommarea").contains(
                "private final static long serialVersionUID = 1L;"));

        globalBindings(internalBindings, "lsfileaq", 123589357872112454L, true);
        assertTrue(getSource("lsfileaq", "DfhCommarea")
                .contains(
                        "private final static long serialVersionUID = 123589357872112454L;"));
    }

    /**
     * Test name transform with external bindings.
     * 
     * @param internalBindings uses internal or external bindings
     */
    public void nameTransform(final boolean internalBindings) {
        nameTransform(internalBindings, "lsfileaq", "SomePrefix", null, null,
                null);
        assertTrue(getSource("lsfileaq", "SomePrefixDfhCommarea").contains(
                "public class SomePrefixDfhcommarea"));

        nameTransform(internalBindings, "lsfileaq", null, "SomeSuffix", null,
                null);
        assertTrue(getSource("lsfileaq", "DfhCommareaSomeSuffix").contains(
                "public class DfhcommareaSomeSuffix"));

        nameTransform(internalBindings, "lsfileaq", "SomePrefix", "SomeSuffix",
                null,
                null);
        assertTrue(getSource("lsfileaq", "SomePrefixDfhCommareaSomeSuffix")
                .contains(
                        "public class SomePrefixDfhcommareaSomeSuffix"));

        nameTransform(internalBindings, "MSNSearch", null, null, "SomePrefix",
                null);
        assertTrue(getSource("MSNSearch", "SomePrefixSearchResponse").contains(
                "public class SomePrefixSearchResponse"));

        nameTransform(internalBindings, "MSNSearch", null, null, null,
                "SomeSuffix");
        assertTrue(getSource("MSNSearch", "SearchResponseSomeSuffix").contains(
                "public class SearchResponseSomeSuffix"));

        nameTransform(internalBindings, "MSNSearch", null, null, "SomePrefix",
                "SomeSuffix");
        assertTrue(getSource("MSNSearch", "SomePrefixSearchResponseSomeSuffix")
                .contains(
                        "public class SomePrefixSearchResponseSomeSuffix"));

    }

    /**
     * A helper method for global bindings tests.
     * 
     * @param internalBindings uses internal or external bindings
     * @param schemaName the schema name
     * @param serializableUid the serial unique ID
     * @param generateIsSetMethod if generate is set methods
     */
    protected void globalBindings(final boolean internalBindings,
            final String schemaName,
            final long serializableUid,
            final boolean generateIsSetMethod) {
        _task.setInternalBindings(internalBindings);
        _task.setXsdFile(getSchemaFromResources(schemaName));
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
     */
    protected void nameTransform(final boolean internalBindings,
            final String schemaName,
            final String typeNamePrefix,
            final String typeNameSuffix,
            final String elementNamePrefix,
            final String elementNameSuffix) {
        _task.setInternalBindings(internalBindings);
        _task.setXsdFile(getSchemaFromResources(schemaName));
        _task.setTargetDir(GEN_SRC_DIR);
        _task.setTypeNamePrefix(typeNamePrefix);
        _task.setTypeNameSuffix(typeNameSuffix);
        _task.setElementNamePrefix(elementNamePrefix);
        _task.setElementNameSuffix(elementNameSuffix);
        _task.setJaxbPackageName("com.legstar.test.coxb." + schemaName);
        _task.execute();
    }
}
