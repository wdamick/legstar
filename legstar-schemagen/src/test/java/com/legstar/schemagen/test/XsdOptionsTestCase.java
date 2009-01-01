/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.schemagen.test;

import com.legstar.schemagen.COB2XSDJNIWrapper;

import junit.framework.TestCase;

/**
 * This test cases class verifies that XML schema generation options are having
 * the expected effect.
 */
public class XsdOptionsTestCase extends TestCase {

    /** XML schema generator. */
    private COB2XSDJNIWrapper mXsdGenerator;

    /** XML schema generator input parameters. */
    private COB2XSDJNIWrapper.InVars mInVars;

    /** XML schema generator output parameters. */
    private COB2XSDJNIWrapper.OutVars mOutVars;

    /** COBOL source test cases. */
    private static final String COB_DIR = "./src/test/cobol/local";

    /** {@inheritDoc} */
    protected final void setUp() throws Exception {
        super.setUp();
        mXsdGenerator = new COB2XSDJNIWrapper();
        mInVars =  mXsdGenerator.new InVars();
        mOutVars =  mXsdGenerator.new OutVars();
        mInVars.debugMode = false;
        mInVars.inRootName = "MyRoot";
        mInVars.xsdOptions = mXsdGenerator.new XsdOptions();
        mInVars.cobolOptions = mXsdGenerator.new CobolOptions();
        java.io.File temp = java.io.File.createTempFile("cob2xsd", ".xsd");

        // Delete temp file when program exits.
        temp.deleteOnExit();

        mInVars.outFile = temp.getAbsolutePath();
        mOutVars.message = "";
    }

    /** {@inheritDoc} */
    protected final void tearDown() throws Exception {
        super.tearDown();
    }

    /** Quick simple pass.
     * @throws Exception if anything goes wrong */
    public final void testDefault() throws Exception {
        mInVars.inFile = COB_DIR + "/simplest.cob";
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
    }

    /** Minus sign replaced with pound.
     * @throws Exception if anything goes wrong   */
    public final void testReplaceMinus() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.removeMinus = false;
        mInVars.xsdOptions.replaceMinus = true;
        mInVars.xsdOptions.replaceMinusChar = "#";
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("W#Minus#Sign")) {
            fail(readXSD());
        }
    }

    /** Minus sign removed (takes precedence over replacement).
     * @throws Exception if anything goes wrong*/
    public final void testRemoveMinus() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.removeMinus = true;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("WMinusSign")) {
            fail(readXSD());
        }
    }

    /** Upper case to lower case.
     * @throws Exception if anything goes wrong*/
    public final void testUpperToLowerTrue() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.removeMinus = true;
        mInVars.xsdOptions.uppercaseToLower = true;
        mInVars.xsdOptions.firstcharUpper = false;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("wminussign")) {
            fail(readXSD());
        }
    }

    /** Upper case stay uppercase.
     * @throws Exception if anything goes wrong*/
    public final void testUpperToLowerFalse() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.removeMinus = true;
        mInVars.xsdOptions.uppercaseToLower = false;
        mInVars.xsdOptions.firstcharUpper = false;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("WMINUSSIGN")) {
            fail(readXSD());
        }
    }

    /** Upper case to lower case.
     * @throws Exception if anything goes wrong*/
    public final void testFirstCharUpperFalse() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.removeMinus = true;
        mInVars.xsdOptions.replaceMinus = false;
        mInVars.xsdOptions.uppercaseToLower = true;
        mInVars.xsdOptions.firstcharUpper = false;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("wminussign")) {
            fail(readXSD());
        }
    }

    /** First char starting a word break should be uppercased.
     * @throws Exception if anything goes wrong*/
    public final void testFirstCharUpperTrue() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.removeMinus = true;
        mInVars.xsdOptions.replaceMinus = false;
        mInVars.xsdOptions.uppercaseToLower = true;
        mInVars.xsdOptions.firstcharUpper = true;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("WMinusSign")) {
            fail(readXSD());
        }
    }

    /** Word break at digit sequence.
     * @throws Exception if anything goes wrong */
    public final void testNameWithNumerics() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.removeMinus = true;
        mInVars.xsdOptions.replaceMinus = false;
        mInVars.xsdOptions.uppercaseToLower = true;
        mInVars.xsdOptions.firstcharUpper = true;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("WImbed95Num")) {
            fail(readXSD());
        }
    }

    /** Complex type names should be suffixed.
     * @throws Exception if anything goes wrong */
    public final void testTypeSuffix() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.inRootName = "TheRoot";
        mInVars.xsdOptions.typeSuffix = "Zeta";
        mInVars.xsdOptions.uppercaseToLower = false;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("complexType name=\"TheRootZeta\"")) {
            fail(readXSD());
        }
    }

    /** Complex type names should not be suffixed.
     * @throws Exception if anything goes wrong */
    public final void testTypeNoSuffix() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.inRootName = "TheRoot";
        mInVars.xsdOptions.typeSuffix = null;
        mInVars.xsdOptions.uppercaseToLower = false;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("complexType name=\"TheRoot\"")) {
            fail(readXSD());
        }
    }

    /** Prevent cobol annotations from being inserted.
     * @throws Exception if anything goes wrong */
    public final void testNoCobolAnnotation() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.xscbPrefix = null;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        assertEquals(-1, readXSD().indexOf("cobolElement"));
    }

    /** Check that we can change the XML namespace prefix.
     * @throws Exception if anything goes wrong */
    public final void testXMLNamespacePrefix() throws Exception {
        mInVars.inFile = COB_DIR + "/simplest.cob";
        mInVars.xsdOptions.xsPrefix = "toz";
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("<toz:schema xmlns:toz")) {
            fail(readXSD());
        }
    }

    /** Check that we can change the XML namespace.
     * @throws Exception if anything goes wrong  */
    public final void testXMLNamespace() throws Exception {
        mInVars.inFile = COB_DIR + "/simplest.cob";
        mInVars.xsdOptions.xsNs = "http://www.w3.org/2003/XMLSchema";
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf(
        "xmlns:xs=\"http://www.w3.org/2003/XMLSchema\"")) {
            fail(readXSD());
        }
    }

    /** Check that we can change the target namespace prefix.
     * @throws Exception if anything goes wrong */
    public final void testTargetNamespacePrefix() throws Exception {
        mInVars.inFile = COB_DIR + "/simplest.cob";
        mInVars.xsdOptions.xsnsPrefix = "toz1";
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf(
        "xmlns:toz1=\"http://tempuri/schemagen\"")) {
            fail(readXSD());
        }
    }

    /** Check that we can change the target namespace.
     * @throws Exception if anything goes wrong  */
    public final void testTargetNamespace() throws Exception {
        mInVars.inFile = COB_DIR + "/simplest.cob";
        mInVars.xsdOptions.xsnsNs = "http://tempureeauxpoix/schemagen";
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf(
        "xmlns:xsns=\"http://tempureeauxpoix/schemagen\"")) {
            fail(readXSD());
        }
    }

    /** Verify that namespace is mandatory when cobol annotations must be
     *  generated.
     * @throws Exception if anything goes wrong */
    public final void testCobolAnnotationNamespace() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.xscbPrefix = "cb";
        mInVars.xsdOptions.xscbNs = null;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(8, resp);
        assertEquals("Missing cobol to XML schema namespace", mOutVars.message);
    }

    /** Verify if cobol annotations gets inserted in generated schema.
     * @throws Exception if anything goes wrong */
    public final void testCobolAnnotation() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.xscbPrefix = "cb";
        mInVars.xsdOptions.xscbNs = "http://www.legsem.com/xml/ns/coxb";
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf(
        "cobolElement levelNumber='01' cobolName='W-MINUS-SIGN'")) {
            fail(readXSD());
        }
    }

    /** Verify that cobol annotations do not get inserted if cobol schema prefix
     *  is not provided.
     * @throws Exception if anything goes wrong */
    public final void testCobolAnnotationNotInserted() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.xscbPrefix = null;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 != readXSD().indexOf(
        "cobolElement levelNumber='01' cobolName='W-MINUS-SIGN'")) {
            fail(readXSD());
        }
    }

    /** Verify that jaxb annotations do not get inserted if jaxb schema prefix
     *  is not provided.
     * @throws Exception if anything goes wrong */
    public final void testJaxbAnnotationNotInserted() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.xsjaxbPrefix = null;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 != readXSD().indexOf("globalBindings generateIsSetMethod")) {
            fail(readXSD());
        }
    }

    /** Verify that namespace, version and package are mandatory when jaxb
     *  annotations must be generated.
     * @throws Exception if anything goes wrong */
    public final void testJaxbAnnotationNamespace() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.xsjaxbPrefix = "jaxb";
        mInVars.xsdOptions.xsjaxbNs = null;
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(8, resp);
        assertEquals("Missing JAXB schema namespace", mOutVars.message);

        mInVars.xsdOptions.xsjaxbNs = "http://java.sun.com/xml/ns/jaxb";
        mInVars.xsdOptions.xsjaxbVersion = "";
        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(8, resp);
        assertEquals("Missing JAXB version", mOutVars.message);

        mInVars.xsdOptions.xsjaxbVersion = "2.0";
        mInVars.xsdOptions.xsjaxbPackage = null;
        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(8, resp);
        assertEquals("Missing JAXB package", mOutVars.message);
    }

    /** Verify if jaxb annotations gets inserted in generated schema.
     * @throws Exception if anything goes wrong */
    public final void testJaxbAnnotation() throws Exception {
        mInVars.inFile = COB_DIR + "/minusSign.cob";
        mInVars.xsdOptions.xsjaxbPrefix = "jaxb";
        mInVars.xsdOptions.xsjaxbVersion = "2.0";
        mInVars.xsdOptions.xsjaxbNs = "http://java.sun.com/xml/ns/jaxb";
        mInVars.xsdOptions.xsjaxbPackage = "com.tempuri";
        int resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(0, resp);
        assertEquals("", mOutVars.message);
        if (-1 == readXSD().indexOf("<jaxb:package  name=\"com.tempuri\"/>")) {
            fail(readXSD());
        }
    }

    /** Turn resulting schema into a string to facilitate searching.
     * @return the XML schema as a string
     * @throws Exception if anything goes wrong */
    private String readXSD() throws Exception {
        java.io.FileInputStream fis = new java.io.FileInputStream(
                mInVars.outFile);
        java.nio.channels.FileChannel fc = fis.getChannel();
        java.nio.MappedByteBuffer mbf
        = fc.map(java.nio.channels.FileChannel.MapMode.READ_ONLY, 0, fc.size());
        byte[] barray = new byte[(int) (fc.size())];
        mbf.get(barray);
        return new String(barray); //one big string
    }
}
