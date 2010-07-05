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
package com.legstar.jaxb.plugin;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Commandline;

import com.legstar.codegen.CodeGenUtil;
import com.sun.tools.xjc.XJC2Task;

import junit.framework.TestCase;

/**
 * Test the JAXB Annotator.
 *
 */
public class CobolJAXBAnnotatorTest extends TestCase {

    /** Target location for generated JAXB classes. */
    private static final File GEN_SRC_DIR = new File("target/src/gen/java");

    /** All java package names will have the same prefix.*/
    private static final String GEN_SRC_SUBDIR = "com/legstar/test/coxb";

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** Make sure we have an output folder.
     * @throws Exception if output folder cannot be created */
    protected void setUp() throws Exception {
        CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
    }
    /**
     * Test various simple type annotations.
     */
    public void testSimpleAnnotation() {
        genSource("ALLTYPES");
        String srce = getSource("ALLTYPES", "DfhCommarea");
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-STRING\","
                + " type = CobolType.ALPHANUMERIC_ITEM,"
                + " levelNumber = 5,"
                + " picture = \"X(4)\","
                + " srceLine = 24)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-BINARY\","
                + " type = CobolType.OCTET_STREAM_ITEM,"
                + " levelNumber = 5,"
                + " picture = \"X(4)\","
                + " srceLine = 25)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-SHORT\","
                + " type = CobolType.BINARY_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = true,"
                + " totalDigits = 4,"
                + " picture = \"S9(4)\","
                + " usage = \"BINARY\","
                + " srceLine = 26)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-USHORT\","
                + " type = CobolType.BINARY_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = false,"
                + " totalDigits = 4,"
                + " picture = \"9(4)\","
                + " usage = \"BINARY\","
                + " srceLine = 27)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-INT\","
                + " type = CobolType.BINARY_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = true,"
                + " totalDigits = 9,"
                + " picture = \"S9(9)\","
                + " usage = \"BINARY\","
                + " srceLine = 28)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-UINT\","
                + " type = CobolType.BINARY_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = false,"
                + " totalDigits = 9,"
                + " picture = \"9(9)\","
                + " usage = \"BINARY\","
                + " srceLine = 29)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-LONG\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = true,"
                + " totalDigits = 18,"
                + " picture = \"S9(18)\","
                + " usage = \"PACKED-DECIMAL\","
                + " srceLine = 30)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-ULONG\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = false,"
                + " totalDigits = 18,"
                + " picture = \"9(18)\","
                + " usage = \"PACKED-DECIMAL\","
                + " srceLine = 31)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-XLONG\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = true,"
                + " totalDigits = 31,"
                + " picture = \"S9(31)\","
                + " usage = \"PACKED-DECIMAL\","
                + " srceLine = 32)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-UXLONG\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = false,"
                + " totalDigits = 31,"
                + " picture = \"9(31)\","
                + " usage = \"PACKED-DECIMAL\","
                + " srceLine = 33)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-DEC\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5,"
                + " isSigned = false,"
                + " totalDigits = 9,"
                + " fractionDigits = 2,"
                + " picture = \"9(7)V99\","
                + " usage = \"PACKED-DECIMAL\","
                + " srceLine = 34)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-FLOAT\","
                + " type = CobolType.SINGLE_FLOAT_ITEM,"
                + " levelNumber = 5,"
                + " usage = \"COMP-1\","
                + " srceLine = 35)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-DOUBLE\","
                + " type = CobolType.DOUBLE_FLOAT_ITEM,"
                + " levelNumber = 5,"
                + " usage = \"COMP-2\","
                + " srceLine = 36)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"A-STRING\","
                + " type = CobolType.ALPHANUMERIC_ITEM,"
                + " levelNumber = 5,"
                + " minOccurs = 2,"
                + " maxOccurs = 2,"
                + " picture = \"X(4)\","
                + " srceLine = 38)"));
    }

    /**
     * Test an enumeration case.
     */
    public void testEnumAnnotation() {
        genSource("MSNSearch");
        String srce = getSource("MSNSearch", "SearchRequestType");
        assertTrue(srce.contains("@CobolElement(cobolName = \"SafeSearch\","
                + " type = CobolType.ALPHANUMERIC_ITEM,"
                + " levelNumber = 5,"
                + " picture = \"X(32)\","
                + " usage = \"DISPLAY\")"));
    }

    /**
     * Test a schema produced by Xsdcgen.
     */
    public void testXsdcgenOutput() {
        genSource("cultureinfo");
        String srce = getSource("cultureinfo", "CultureInfoParameters");
        assertTrue(srce.contains("@CobolElement(cobolName = \"cultureCode\","
                + " type = CobolType.ALPHANUMERIC_ITEM,"
                + " levelNumber = 5,"
                + " picture = \"X(32)\","
                + " usage = \"DISPLAY\")"));
    }

    /**
     * Test a schema produced by Xsdcgen with binding to a POJO.
     */
    public void testXsdcgenOutputWithJavaClassNames() {
        genSource("jvmquery");
        String srce = getSource("jvmquery", "JvmQueryReply");
        assertTrue(srce.contains(
        "@CobolComplexType(javaClassName = \"com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply\")"));
    }

    /**
     * Test default value case.
     */
    public void testDefaultValues() {
        genSource("valuemix");
        String srce = getSource("valuemix", "Dfhcommarea");
        assertTrue(srce.contains("protected long wsZero = 0L;"));
        assertTrue(srce.contains("protected long wsZeros = 0L;"));
        assertTrue(srce.contains("protected long wsZeroes = 0L;"));
        assertTrue(srce.contains("protected String wsSpace = \"\";"));
        assertTrue(srce.contains("protected String wsSpaces = \"\";"));
        assertTrue(srce.contains("protected String wsHighValue = \"0xFFFFFFFFFF\";"));
        assertTrue(srce.contains("protected String wsHighValues = \"0xFFFFFFFFFF\";"));
        assertTrue(srce.contains("protected String wsLowValue = \"0x0000000000\";"));
        assertTrue(srce.contains("protected String wsLowValues = \"0x0000000000\";"));
        assertTrue(srce.contains("protected String wsQuote = \"\\'\";"));
        assertTrue(srce.contains("protected String wsQuotes = \"\\'\";"));
        assertTrue(srce.contains("protected String wsNull = \"0x0000000000\";"));
        assertTrue(srce.contains("protected String wsNulls = \"0x0000000000\";"));
        assertTrue(srce.contains("protected String wsString = \"ABCDE\";"));
        assertTrue(srce.contains("protected int wsNumeric = -345;"));
        assertTrue(srce.contains("protected BigDecimal wsPackedDecimal = (new BigDecimal(\"-245.56\"));"));
        assertTrue(srce.contains("protected float wsSingleFloat = 6.0E7F;"));
        assertTrue(srce.contains("protected double wsDoubleFloat = -1.8E-56D;"));
    }

    /**
     * Test a case with a short integer that should be initialized.
     */
    public void testInitializeIntegers() {
        genSource("lsfileaq");
        String srce = getSource("lsfileaq", "QueryData");
        assertTrue(srce.contains("protected short maxReplies = -1"));
    }

    /**
     * Generates JAXB classes with Cobol annotations.
     * @param schemaName the schema used to generate
     */
    private void genSource(final String schemaName) {
        Project project = new Project();
        project.setBasedir(".");
        DefaultLogger logger = new DefaultLogger();
        logger.setErrorPrintStream(System.out);
        project.addBuildListener(logger);
        XJC2Task xjcTask = new XJC2Task();
        xjcTask.setProject(project);
        xjcTask.setTaskName("xjcTask");
        xjcTask.setSchema(getSchemaFromResources(schemaName).getAbsolutePath());
        xjcTask.setDestdir(GEN_SRC_DIR);
        xjcTask.setExtension(true);
        xjcTask.setRemoveOldOutput(true);
        Commandline.Argument arg1 = xjcTask.createArg();
        arg1.setValue("-Xlegstar-code");
        Commandline.Argument arg2 = xjcTask.createArg();
        arg2.setValue("-nv");
        xjcTask.log("Started");
        xjcTask.execute();
    }

    /**
     * Lookup a test schema from the classpath and copies the content to
     * a temporary file. This is because the XJC task does not take a URL
     * directly, it wants a file name for the schema.
     * We have been sometimes inconsistent in naming schemas so we try with
     * multiple cases.
     * @param schemaName the schema name
     * @return the temporary file holding the content
     */
    protected File getSchemaFromResources(final String schemaName) {
        try {
            File tempFile = File.createTempFile("jaxb-schema", "tmp");
            tempFile.deleteOnExit();
            URL resURL = getClass().getResource("/schema/" + schemaName + ".xsd");
            if (resURL == null) {
                if (Character.isLowerCase(schemaName.charAt(0))) {
                    resURL = getClass().getResource("/schema/" + schemaName.toUpperCase() + ".xsd");
                } else {
                    resURL = getClass().getResource("/schema/" + schemaName.toLowerCase() + ".xsd");
                }
            }
            FileUtils.copyURLToFile(resURL, tempFile);
            return tempFile;
        } catch (IOException e) {
            e.printStackTrace();
            fail(e.getMessage());
            return null;
        }
    }

    /**
     * Reads a complete source file into a string.
     * @param schemaName the schema used to generate
     * @param className the generated class name
     * @return a String with the class content
     */
    private String getSource(final String schemaName, final String className) {
        File srcFile = new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + '/' + schemaName + '/' + className + ".java");
        try {
            String result = FileUtils.readFileToString(srcFile);
            if (_log.isDebugEnabled()) {
                _log.debug(srcFile);
            }
            return result;
        } catch (IOException e) {
            fail("Source file " + srcFile.toString() + " was not generated");
            return null;
        }
    }

}
