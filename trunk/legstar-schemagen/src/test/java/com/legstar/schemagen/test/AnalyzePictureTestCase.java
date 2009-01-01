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

import com.legstar.schemagen.COBDDANPJNIWrapper;
import junit.framework.TestCase;

/**
 * This series of tests applies to the code that analyzes COBOL PICTURE clauses.
 */
public class AnalyzePictureTestCase extends TestCase {

    /** Instance of the picture analyzer. */
    private COBDDANPJNIWrapper mPicAnalyzer;

    /** Class describing the input variables. */
    private COBDDANPJNIWrapper.InVars mInVars;

    /** Class describing the output variables. */
    private COBDDANPJNIWrapper.OutVars mOutVars;

    /** Characters only.                          */
    private static final int ALPHABETIC_ITEM = 0;
    /** PIC N when USAGE NATIONAL is specified.   */
    private static final int NATIONAL_ITEM = 1;
    /** PIC Contains G, G and B, or N.            */
    private static final int DBCS_ITEM = 2;
    /** PIC X or A with some B,0 or /.       */
    private static final int ALPHABETIC_EDITED_ITEM = 3;
    /** The usual PIC X or 9 A combination.       */
    private static final int ALPHANUMERIC_ITEM = 4;
    /** PIC X containing binary data.             */
    //private static final int OCTET_STREAM_ITEM = 5;
    /** Single precision floating point.          */
    private static final int SINGLE_FLOAT_ITEM = 6;
    /** Double precision floating point.         */
    private static final int DOUBLE_FLOAT_ITEM = 7;
    /** Compressed decimal (Comp-3, packed).      */
    private static final int PACKED_DECIMAL_ITEM = 8;
    /** Simple display numeric.                 */
    private static final int ZONED_DECIMAL_ITEM = 9;
    /** Display numeric with editing pattern.     */
    private static final int NUMERIC_EDITED_ITEM = 10;
    /** A 4 bytes index.                          */
    //private static final int INDEX_ITEM = 11;
    /** A 4 bytes pointer to a memory block.      */
    //private static final int POINTER_ITEM  = 12;
    /** A 8 bytes pointer to a procedure. */
    //private static final int PROC_POINTER_ITEM = 13;
    /** A 4 bytes pointer to a function. */
    //private static final int FUNC_POINTER_ITEM = 14;
    /** Points to class name (OO extensions)     */
    //private static final int OBJECT_ITEM = 15;
    /** +/- mantissa E +/- exponent.         */
    private static final int EXTERNAL_FLOATING_ITEM = 16;
    /** Binary, Comp, Comp-4, Comp_5 (Big Endian). */
    private static final int BINARY_ITEM = 17;
    /** COMP-5 or binary_item when trunc=bin.     */
    private static final int NATIVE_BINARY_ITEM = 18;
    /** Other item types.                        */
    //private static final int unknown = 19;

    /** Simple boolean simulation.         */
    private static final int TRUE =  1;
    /** Simple boolean simulation.         */
    private static final int FALSE = 0;

    /** {@inheritDoc} */
    protected final void setUp() throws Exception {
        super.setUp();
        mPicAnalyzer = new COBDDANPJNIWrapper();
        mInVars = mPicAnalyzer.new InVars();
        mInVars.debugMode = false;
        mOutVars = mPicAnalyzer.new OutVars();

    }

    /** {@inheritDoc} */
    protected final void tearDown() throws Exception {
        super.tearDown();
    }

    /** Test PIC X(5)..  */
    public final void testAnapict() {

        mInVars.picture = "X(5)";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;

        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(ALPHANUMERIC_ITEM, mOutVars.dataType);
        assertEquals(0, mOutVars.totalDigits);
        assertEquals(0, mOutVars.fractionDigits);
        assertEquals(FALSE, mOutVars.sign);
        assertEquals(5, mOutVars.byteLength);
    }

    /** Test PIC CRZ(45)V9(7)..  */
    public final void testNumericEdited1() {
        mInVars.picture = "CRZ(45)V9(7)";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(52, mOutVars.totalDigits);
        assertEquals(7, mOutVars.fractionDigits);
        assertEquals(54, mOutVars.byteLength);
    }

    /** Test PIC +999.99E+99..  */
    public final void testNumericEdited2() {
        mInVars.picture = "+999.99E+99";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(EXTERNAL_FLOATING_ITEM, mOutVars.dataType);
        assertEquals(5, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(11, mOutVars.byteLength);
    }

    /** Test PIC $9999.99CR..  */
    public final void testNumericEdited3() {
        mInVars.picture = "$9999.99CR";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(6, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(10, mOutVars.byteLength);
    }

    /** Test PIC 999.99+..  */
    public final void testNumericEdited4() {
        mInVars.picture = "999.99+";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(5, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(7, mOutVars.byteLength);
    }

    /** Test PIC $$$$.99..  */
    public final void testNumericEdited5() {
        mInVars.picture = "$$$$.99";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(FALSE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(5, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(7, mOutVars.byteLength);
    }

    /** Test PIC +,+++,999.99..  */
    public final void testNumericEdited6() {
        mInVars.picture = "+,+++,999.99";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(8, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(12, mOutVars.byteLength);
    }

    /** Test PIC $$,$$$,$$$.99CR..  */
    public final void testNumericEdited7() {
        mInVars.picture = "$$,$$$,$$$.99CR";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(9, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(15, mOutVars.byteLength);
    }

    /** Test PIC ****.**..  */
    public final void testNumericEdited8() {
        mInVars.picture = "****.**";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(FALSE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(6, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(7, mOutVars.byteLength);
    }

    /** Test PIC ZZ99.99..  */
    public final void testNumericEdited9() {
        mInVars.picture = "ZZ99.99";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(FALSE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(6, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(7, mOutVars.byteLength);
    }

    /** Test PIC $Z,ZZZ,ZZZ.ZZCR..  */
    public final void testNumericEdited10() {
        mInVars.picture = "$Z,ZZZ,ZZZ.ZZCR";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(9, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(15, mOutVars.byteLength);
    }

    /** Test PIC $B*,***,***.**BBDB..  */
    public final void testNumericEdited11() {
        mInVars.picture = "$B*,***,***.**BBDB";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(NUMERIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(12, mOutVars.totalDigits);
        assertEquals(4, mOutVars.fractionDigits);
        assertEquals(18, mOutVars.byteLength);
    }

    /** Test PIC A(3)X(7)/AAAA9(3)BB0(5)..  */
    public final void testAlphaNumericEdited1() {
        mInVars.picture = "A(3)X(7)/AAAA9(3)BB0(5)";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(ALPHABETIC_EDITED_ITEM, mOutVars.dataType);
        assertEquals(0, mOutVars.totalDigits);
        assertEquals(0, mOutVars.fractionDigits);
        assertEquals(25, mOutVars.byteLength);
    }

    /** Test PIC GGBBGG..  */
    public final void testAlphaNumericEditedDBCS1() {
        mInVars.picture = "GGBBGG";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(DBCS_ITEM, mOutVars.dataType);
        assertEquals(0,  mOutVars.fractionDigits);
        assertEquals(12, mOutVars.byteLength);
        /* Analyzer should have fixed the USAGE */
        assertEquals("DISPLAY-1", mInVars.usage);
    }

    /** Test PIC N(15)..  */
    public final void testAlphaNumericNational1() {
        mInVars.picture = "N(15)";
        mInVars.usage = "NATIONAL";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(NATIONAL_ITEM, mOutVars.dataType);
        assertEquals(0, mOutVars.totalDigits);
        assertEquals(0, mOutVars.fractionDigits);
        assertEquals(30, mOutVars.byteLength);
    }

    /** Test PIC 9(15)V9(4) COMP-3..  */
    public final void testPackedDecimal1() {
        mInVars.picture = "9(15)V9(4)";
        mInVars.usage = "PACKED-DECIMAL";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(FALSE, mOutVars.sign);
        assertEquals(PACKED_DECIMAL_ITEM, mOutVars.dataType);
        assertEquals(19, mOutVars.totalDigits);
        assertEquals(4, mOutVars.fractionDigits);
        assertEquals(10, mOutVars.byteLength);
    }

    /** Test PIC -9V9(15) COMP-3..  */
    public final void testPackedDecimal2() {
        mInVars.picture = "-9V9(15)";
        mInVars.usage = "PACKED-DECIMAL";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(PACKED_DECIMAL_ITEM, mOutVars.dataType);
        assertEquals(16, mOutVars.totalDigits);
        assertEquals(15, mOutVars.fractionDigits);
        assertEquals(9, mOutVars.byteLength);
    }

    /** Test PIC S9V9(15)..  */
    public final void testZonedDecimal1() {
        mInVars.picture = "S9V9(15)";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = true;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(ZONED_DECIMAL_ITEM, mOutVars.dataType);
        assertEquals(16, mOutVars.totalDigits);
        assertEquals(15, mOutVars.fractionDigits);
        assertEquals(17, mOutVars.byteLength);
    }

    /** Test PIC PPP999..  */
    public final void testZonedDecimalPCase1() {
        mInVars.picture = "PPP999";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = true;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(FALSE, mOutVars.sign);
        assertEquals(ZONED_DECIMAL_ITEM, mOutVars.dataType);
        assertEquals(3, mOutVars.totalDigits);
        assertEquals(3, mOutVars.fractionDigits);
        assertEquals(4, mOutVars.byteLength);
    }

    /** Test PIC SPPP999.  */
    public final void testZonedDecimalPCase2() {
        mInVars.picture = "SPPP999";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(ZONED_DECIMAL_ITEM, mOutVars.dataType);
        assertEquals(3, mOutVars.totalDigits);
        assertEquals(0, mOutVars.fractionDigits);
        assertEquals(3, mOutVars.byteLength);
    }

    /** Test PIC S9PPP.  */
    public final void testBinary() {
        mInVars.picture = "S9PPP";
        mInVars.usage = "BINARY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(TRUE, mOutVars.sign);
        assertEquals(BINARY_ITEM, mOutVars.dataType);
        assertEquals(1, mOutVars.totalDigits);
        assertEquals(0, mOutVars.fractionDigits);
        assertEquals(2, mOutVars.byteLength);
    }

    /** Test PIC 9(5)V99.  */
    public final void testBinary2() {
        mInVars.picture = "9(5)V99";
        mInVars.usage = "COMP-5";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(FALSE, mOutVars.sign);
        assertEquals(NATIVE_BINARY_ITEM, mOutVars.dataType);
        assertEquals(7, mOutVars.totalDigits);
        assertEquals(2, mOutVars.fractionDigits);
        assertEquals(4, mOutVars.byteLength);
    }

    /** Test PIC COMP-1).  */
    public final void testSingleFloat() {
        mInVars.usage = "COMP-1";
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(SINGLE_FLOAT_ITEM, mOutVars.dataType);
        assertEquals(4, mOutVars.byteLength);
    }

    /** Test PIC COMP-2).  */
    public final void testDoubleFloat() {
        mInVars.usage = "COMP-2";
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(DOUBLE_FLOAT_ITEM, mOutVars.dataType);
        assertEquals(8, mOutVars.byteLength);
    }

    /** Test PIC A(256).  */
    public final void testAlphabetic() {
        mInVars.picture = "A(256)";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = false;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(0, resp);
        assertEquals(ALPHABETIC_ITEM, mOutVars.dataType);
        assertEquals(0, mOutVars.totalDigits);
        assertEquals(0, mOutVars.fractionDigits);
        assertEquals(256, mOutVars.byteLength);
    }

    /** Test PIC S9V9(.  */
    public final void testInvalidPicture() {
        mInVars.picture = "S9V9(";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = true;
        int resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(8, resp);
        assertEquals("PIC unbalanced parenthesis", mOutVars.message);

        mInVars.picture = "(9)V99";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = true;
        resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(8, resp);
        assertEquals("PIC starting with parenthesis", mOutVars.message);

        mInVars.picture = "9()V99";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = true;
        resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(8, resp);
        assertEquals("PIC empty parenthesis", mOutVars.message);

        mInVars.picture = "9(Z)V99";
        mInVars.usage = "DISPLAY";
        mInVars.signSeparate = true;
        resp = mPicAnalyzer.anapict(mInVars, mOutVars);
        assertEquals(8, resp);
        assertEquals("PIC invalid char between parenthesis", mOutVars.message);
    }
}
