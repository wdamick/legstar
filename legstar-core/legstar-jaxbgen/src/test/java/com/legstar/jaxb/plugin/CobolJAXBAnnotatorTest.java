/*******************************************************************************
 * Copyright (c) 2011 LegSem.
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

import org.apache.commons.io.FileUtils;
import org.apache.tools.ant.Project;

import com.legstar.jaxb.AbstractJaxbGenTest;
import com.legstar.jaxb.gen.CobolJAXBGenerator;

/**
 * Test the JAXB Annotator.
 * 
 */
public class CobolJAXBAnnotatorTest extends AbstractJaxbGenTest {

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
     * Test various simple type annotations.
     * 
     * @throws Exception if generation fails
     */
    public void testSimpleAnnotation() throws Exception {
        genSource("alltypes", "ALLTYPES.xsd");
        String srce = getJaxbSource("alltypes", "Dfhcommarea");
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-STRING\","
                + " type = CobolType.ALPHANUMERIC_ITEM," + " levelNumber = 5,"
                + " picture = \"X(4)\"," + " srceLine = 24)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-BINARY\","
                + " type = CobolType.OCTET_STREAM_ITEM," + " levelNumber = 5,"
                + " picture = \"X(4)\"," + " srceLine = 25)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-SHORT\","
                + " type = CobolType.BINARY_ITEM," + " levelNumber = 5,"
                + " isSigned = true," + " totalDigits = 4,"
                + " picture = \"S9(4)\"," + " usage = \"BINARY\","
                + " srceLine = 26)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-USHORT\","
                + " type = CobolType.BINARY_ITEM," + " levelNumber = 5,"
                + " isSigned = false," + " totalDigits = 4,"
                + " picture = \"9(4)\"," + " usage = \"BINARY\","
                + " srceLine = 27)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-INT\","
                + " type = CobolType.BINARY_ITEM," + " levelNumber = 5,"
                + " isSigned = true," + " totalDigits = 9,"
                + " picture = \"S9(9)\"," + " usage = \"BINARY\","
                + " srceLine = 28)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-UINT\","
                + " type = CobolType.BINARY_ITEM," + " levelNumber = 5,"
                + " isSigned = false," + " totalDigits = 9,"
                + " picture = \"9(9)\"," + " usage = \"BINARY\","
                + " srceLine = 29)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-LONG\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5," + " isSigned = true,"
                + " totalDigits = 18," + " picture = \"S9(18)\","
                + " usage = \"PACKED-DECIMAL\"," + " srceLine = 30)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-ULONG\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5," + " isSigned = false,"
                + " totalDigits = 18," + " picture = \"9(18)\","
                + " usage = \"PACKED-DECIMAL\"," + " srceLine = 31)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-XLONG\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5," + " isSigned = true,"
                + " totalDigits = 31," + " picture = \"S9(31)\","
                + " usage = \"PACKED-DECIMAL\"," + " srceLine = 32)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-UXLONG\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5," + " isSigned = false,"
                + " totalDigits = 31," + " picture = \"9(31)\","
                + " usage = \"PACKED-DECIMAL\"," + " srceLine = 33)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-DEC\","
                + " type = CobolType.PACKED_DECIMAL_ITEM,"
                + " levelNumber = 5," + " isSigned = false,"
                + " totalDigits = 9," + " fractionDigits = 2,"
                + " picture = \"9(7)V99\"," + " usage = \"PACKED-DECIMAL\","
                + " srceLine = 34)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-FLOAT\","
                + " type = CobolType.SINGLE_FLOAT_ITEM," + " levelNumber = 5,"
                + " usage = \"COMP-1\"," + " srceLine = 35)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"S-DOUBLE\","
                + " type = CobolType.DOUBLE_FLOAT_ITEM," + " levelNumber = 5,"
                + " usage = \"COMP-2\"," + " srceLine = 36)"));
        assertTrue(srce.contains("@CobolElement(cobolName = \"A-STRING\","
                + " type = CobolType.ALPHANUMERIC_ITEM," + " levelNumber = 5,"
                + " minOccurs = 2," + " maxOccurs = 2,"
                + " picture = \"X(4)\"," + " srceLine = 38)"));
    }

    /**
     * Test an enumeration case.
     * 
     * @throws Exception if generation fails
     */
    public void testEnumAnnotation() throws Exception {
        genSource("MSNSearch", "MSNSearch.xsd", "Type");
        String srce = getJaxbSource("MSNSearch", "SearchRequestType");
        assertTrue(srce.contains("@CobolElement(cobolName = \"SafeSearch\","
                + " type = CobolType.ALPHANUMERIC_ITEM," + " levelNumber = 5,"
                + " picture = \"X(32)\"," + " usage = \"DISPLAY\")"));
    }

    /**
     * Test a schema produced by Xsdcgen.
     * 
     * @throws Exception if generation fails
     */
    public void testXsdcgenOutput() throws Exception {
        genSource("cultureinfo", "cultureinfo.xsd");
        String srce = getJaxbSource("cultureinfo", "CultureInfoParameters");
        assertTrue(srce.contains("@CobolElement(cobolName = \"cultureCode\","
                + " type = CobolType.ALPHANUMERIC_ITEM," + " levelNumber = 5,"
                + " picture = \"X(32)\"," + " usage = \"DISPLAY\")"));
    }

    /**
     * Test a schema produced by Xsdcgen with binding to a POJO.
     * 
     * @throws Exception if generation fails
     */
    public void testXsdcgenOutputWithJavaClassNames() throws Exception {
        genSource("jvmquery", "jvmquery.xsd");
        String srce = getJaxbSource("jvmquery", "JvmQueryReply");
        assertTrue(srce
                .contains("@CobolComplexType(javaClassName = \"com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply\")"));
    }

    /**
     * Test default value case.
     * 
     * @throws Exception if generation fails
     */
    public void testDefaultValues() throws Exception {
        genSource("valuemix", "VALUEMIX.xsd");
        String srce = getJaxbSource("valuemix", "Dfhcommarea");
        assertTrue(srce.contains("protected long wsZero = 0L;"));
        assertTrue(srce.contains("protected long wsZeros = 0L;"));
        assertTrue(srce.contains("protected long wsZeroes = 0L;"));
        assertTrue(srce.contains("protected String wsSpace = \"\";"));
        assertTrue(srce.contains("protected String wsSpaces = \"\";"));
        assertTrue(srce
                .contains("protected String wsHighValue = \"0xFFFFFFFFFF\";"));
        assertTrue(srce
                .contains("protected String wsHighValues = \"0xFFFFFFFFFF\";"));
        assertTrue(srce
                .contains("protected String wsLowValue = \"0x0000000000\";"));
        assertTrue(srce
                .contains("protected String wsLowValues = \"0x0000000000\";"));
        assertTrue(srce.contains("protected String wsQuote = \"\'\";"));
        assertTrue(srce.contains("protected String wsQuotes = \"\'\";"));
        assertTrue(srce.contains("protected String wsNull = \"0x0000000000\";"));
        assertTrue(srce
                .contains("protected String wsNulls = \"0x0000000000\";"));
        assertTrue(srce.contains("protected String wsString = \"ABCDE\";"));
        assertTrue(srce.contains("protected int wsNumeric = -345;"));
        assertTrue(srce
                .contains("protected BigDecimal wsPackedDecimal = (new BigDecimal(\"-245.56\"));"));
        assertTrue(srce.contains("protected float wsSingleFloat = 6.0E7F;"));
        assertTrue(srce.contains("protected double wsDoubleFloat = -1.8E-56D;"));
    }

    /**
     * Test a case with a short integer that should be initialized.
     * 
     * @throws Exception if generation fails
     */
    public void testInitializeIntegers() throws Exception {
        genSource("lsfileaq", "LSFILEAQ.xsd");
        String srce = getJaxbSource("lsfileaq", "QueryData");
        assertTrue(srce.contains("protected short maxReplies = -1"));
    }

    /**
     * Test the ECI compatibility mode.
     * 
     * @throws Exception if test fails
     */
    public void testEciCompatibleNameConverter() throws Exception {

        _task.setEciCompatible(true);

        File tempFile = File.createTempFile(getName(), ".tmp");
        tempFile.deleteOnExit();
        FileUtils
                .writeStringToFile(
                        tempFile,
                        "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\""
                                + "  xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\""
                                + "  attributeFormDefault=\"unqualified\" elementFormDefault=\"unqualified\">"
                                + "    <xs:complexType name=\"CustomerType\">"
                                + "        <xs:sequence>"
                                + "            <xs:element name=\"name_ab_cd\" type=\"xs:string\">"
                                + "                <xs:annotation>"
                                + "                    <xs:appinfo>"
                                + "                        <cb:cobolElement cobolName=\"name-ab-cd\" levelNumber=\"3\" picture=\"X(32)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"
                                + "                    </xs:appinfo>"
                                + "                </xs:annotation>"
                                + "            </xs:element>"
                                + "        </xs:sequence>"
                                + "    </xs:complexType>" + "</xs:schema>"

                );
        genSource("testNaming", tempFile, null);
        String srce = getJaxbSource("testNaming", "CustomerType");
        assertTrue(srce.contains("public class CustomerType"));
        assertTrue(srce.contains("public String getName_ab_cd() {"));
        assertTrue(srce.contains("protected String name_ab_cd;"));
    }

    /**
     * Test the Comn root name issue.
     * 
     * @throws Exception if test fails
     */
    public void testComnRootName() throws Exception {

        File tempFile = File.createTempFile(getName(), ".tmp");
        tempFile.deleteOnExit();
        FileUtils
                .writeStringToFile(
                        tempFile,
                        "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\""
                                + "  xmlns:cb=\"http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd\""
                                + "  attributeFormDefault=\"unqualified\" elementFormDefault=\"unqualified\">"
                                + "    <xs:complexType name=\"Com1\">"
                                + "        <xs:sequence>"
                                + "            <xs:element name=\"a\" type=\"xs:string\">"
                                + "                <xs:annotation>"
                                + "                    <xs:appinfo>"
                                + "                        <cb:cobolElement cobolName=\"A\" levelNumber=\"3\" picture=\"X(32)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"
                                + "                    </xs:appinfo>"
                                + "                </xs:annotation>"
                                + "            </xs:element>"
                                + "        </xs:sequence>"
                                + "    </xs:complexType>" + "</xs:schema>"

                );
        genSource("testComnRootName", tempFile, null);
        String srce = getJaxbSource("testComnRootName", "Com1w");
        assertTrue(srce.contains("public class Com1"));
        assertTrue(srce.contains("public String getA() {"));
        assertTrue(srce.contains("protected String a;"));
    }

    /**
     * Generates JAXB classes with Cobol annotations.
     * 
     * @param schemaName the schema used to generate
     * @param schemaFileName the schema file name
     * @throws Exception if generation fails
     */
    private void genSource(final String schemaName, final String schemaFileName)
            throws Exception {
        genSource(schemaName, schemaFileName, null);
    }

    /**
     * Generates JAXB classes with Cobol annotations.
     * 
     * @param schemaName the schema used to generate
     * @param schemaFileName the schema file name
     * @param typeNameSuffix the type name suffix if needed
     * @throws Exception if generation fails
     */
    private void genSource(final String schemaName,
            final String schemaFileName, final String typeNameSuffix)
            throws Exception {
        genSource(schemaName, new File(COB_XSD_DIR, schemaFileName),
                typeNameSuffix);
    }

    /**
     * Generates JAXB classes with Cobol annotations.
     * 
     * @param schemaName the schema used to generate
     * @param typeNameSuffix the type name suffix if needed
     * @throws Exception if generation fails
     */
    private void genSource(final String schemaName, final File schemaFile,
            final String typeNameSuffix) throws Exception {
        _task.setInternalBindings(true);
        _task.setXsdFile(schemaFile);
        _task.setTargetDir(GEN_SRC_DIR);
        _task.setTypeNameSuffix(typeNameSuffix);
        _task.setJaxbPackageName(JAXB_PKG_PFX + "." + schemaName);
        _task.execute();
    }
}
