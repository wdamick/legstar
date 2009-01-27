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
package com.legstar.schemagen.test;


import com.legstar.schemagen.COB2XSDJNIWrapper;

import junit.framework.TestCase;

/**
 * This test cases class verifies that cobol options are having the expected 
 * effect on XML schema generation.
 */
public class CobolOptionsCases extends TestCase {

    /** XML schema generator. */
    private COB2XSDJNIWrapper mXsdGenerator;

    /** XML schema generator input parameters. */
    private COB2XSDJNIWrapper.InVars mInVars;

    /** XML schema generator output parameters. */
    private COB2XSDJNIWrapper.OutVars mOutVars;

    /** COBOL source test cases. */
    private static final String COB_DIR = "src/test/cobol/local";

    /** {@inheritDoc} */
    protected final void setUp() throws Exception {
        super.setUp();
        mXsdGenerator = new COB2XSDJNIWrapper();
        mInVars =  mXsdGenerator.new InVars();
        mOutVars =  mXsdGenerator.new OutVars();
        mInVars.debugMode = false;
        mInVars.inRootName = "MyRoot";
        mInVars.cobolOptions = mXsdGenerator.new CobolOptions();
        mOutVars.message = "";
        java.io.File temp = java.io.File.createTempFile("cob2xsd", ".xsd");

        // Delete temp file when program exits.
        temp.deleteOnExit();

        mInVars.outFile = temp.getAbsolutePath();
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
        assertEquals(mOutVars.message, 0, resp);
    }

    /** Check for proper handling of invalid input source file.
     * @throws Exception if anything goes wrong */
    public final void testWithUnknowncobolSourceFile() throws Exception {
        int resp;
        mInVars.inFile = null;
        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals("Missing input file", mOutVars.message);
        assertEquals(8, resp);
        mInVars.inFile = "tartempion/unreal.zut";
        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals("Invalid input file tartempion/unreal.zut",
                mOutVars.message);
        assertEquals(8, resp);
    }

    /** If we pass no cobol options, there should be a default.
     * @throws Exception if anything goes wrong  */
    public final void testDefaultCobolOptions() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/simplest.cob";
        /* Make sure, a debug line gets selected */
        mInVars.cobolOptions = null;
        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals("", mOutVars.message);
        assertEquals(mOutVars.message, 0, resp);
    }

    /** Cobol debug lines can be treated or ignored.
     * @throws Exception if anything goes wrong */
    public final void testDebugLinesTrue() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/debugLines.cob";
        /* Make sure, a debug line gets selected */
        mInVars.cobolOptions.includeDebugLines = true;
        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        if (-1 == readXSD().indexOf("WsDebugLine")) {
            fail(readXSD());
        }
    }

    /** Cobol debug lines can be treated or ignored.
     * @throws Exception if anything goes wrong */
    public final void testDebugLinesFalse() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/debugLines.cob";
        /* Make sure, a debug line is ignored */
        mInVars.cobolOptions.includeDebugLines = false;
        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        assertEquals(-1, readXSD().indexOf("WsDebugLine"));
    }

    /** Test that currency sign can be changed.
     * @throws Exception if anything goes wrong */
    public final void testCurrencySign() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/currencySign.cob";
        /* Set compiler options to selected currency sign */
        mInVars.cobolOptions.currencySign = "€";

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
    }

    /** Test that unsupported currency sign results in error.
     * @throws Exception if anything goes wrong */
    public final void testWrongCurrencySign() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/currencySign.cob";
        /* Set compiler options to selected currency sign */
        mInVars.cobolOptions.currencySign = "$";
        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals("Line=5, Unrecognized symbol € in picture clause",
                mOutVars.message);
        assertEquals(8, resp);
    }

    /** When TRUNC(BIN) is in effect all binary numerics are considered native.
     * @throws Exception if anything goes wrong */
    public final void testTruncBinOn() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/truncBin.cob";
        mInVars.cobolOptions.truncBin = true;

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        if (-1 == readXSD().indexOf("type='NATIVE_BINARY_ITEM'")) {
            fail(readXSD());
        }
    }

    /** When TRUNC(BIN) is not in effect COMP numerics are not native.
     * @throws Exception if anything goes wrong */
    public final void testTruncBinOff() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/truncBin.cob";
        mInVars.cobolOptions.truncBin = false;

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        if (-1 == readXSD().indexOf("type='BINARY_ITEM'")) {
            fail(readXSD());
        }
    }

    /** When decimal point is comma, fractional digits start at the comma sign
     * a numeric such as +9(5),99 should not 2 fractionDigits.
     * @throws Exception if anything goes wrong */
    public final void testDecimalPointIsComma() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/decimalPointComma.cob";
        mInVars.cobolOptions.decimalPointIsComma = true;

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        if (-1 == readXSD().indexOf("fractionDigits='2'")) {
            fail(readXSD());
        }
    }

    /** When decimal point is point, fractional digits start at the point sign.
     * a numeric such as +9(5),99 should not have fractionDigits.
     * @throws Exception if anything goes wrong */
    public final void testDecimalPointIsPoint() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/decimalPointComma.cob";
        mInVars.cobolOptions.decimalPointIsComma = false;

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        assertEquals(-1, readXSD().indexOf("fractionDigits"));
    }

    /** With NSYMBOL(DBCS), data descriptions declared as PIC N should be
     * considered DBCS and their usage should be DISPLAY-1.
     * @throws Exception if anything goes wrong */
    public final void testNSymbolDBCS() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/nsymbolDbcs.cob";
        mInVars.cobolOptions.nsymbolDbcs = true;

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        if (-1 == readXSD().indexOf(
        "type='DBCS_ITEM' picture='N(10)' usage='DISPLAY-1'")) {
            fail(readXSD());
        }
    }

    /** Without NSYMBOL(DBCS), data descriptions declared as PIC N are NATIONAL.
     * @throws Exception if anything goes wrong */
    public final void testNSymbolNATIONAL() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/nsymbolDbcs.cob";
        mInVars.cobolOptions.nsymbolDbcs = false;

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        if (-1 == readXSD().indexOf(
        "type='NATIONAL_ITEM' picture='N(10)' usage='NATIONAL'")) {
            fail(readXSD());
        }
    }

    /** Without QUOTE, string delimiters should be apostrophes.
     * @throws Exception if anything goes wrong */
    public final void testQuoteTrue() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/valueQuotes.cob";
        mInVars.cobolOptions.quote = true;

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        assertEquals(mOutVars.message, 0, resp);
        if (-1 == readXSD().indexOf("<cb:value>&quote;</cb:value>")) {
            fail(readXSD());
        }
    }

    /** Without QUOTE, string delimiters should be apostrophes.
     * @throws Exception if anything goes wrong */
    public final void testQuoteFalse() throws Exception {
        int resp;
        mInVars.inFile = COB_DIR + "/valueQuotes.cob";
        mInVars.cobolOptions.quote = false;

        resp = mXsdGenerator.cob2xsd(mInVars, mOutVars);
        if (mInVars.debugMode) {
            System.out.println(readXSD());
        }
        assertEquals(mOutVars.message, 0, resp);
        if (-1 == readXSD().indexOf("<cb:value>&apost;</cb:value>")) {
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
