package com.legstar.coxb.impl;

import com.legstar.coxb.CobolType;

import junit.framework.TestCase;

/**
 * Test the capability to evaluate an element host byte size.
 */
public class CalcByteLengthTest extends TestCase {
    
    /**
     * String case.
     */
    public void testCStringBinding() {
        assertEquals(14, CStringBinding.calcStringByteLength("A(2)X(3)/9900BBB/", CobolType.ALPHANUMERIC_EDITED_ITEM));
        assertEquals(10, CStringBinding.calcStringByteLength("+99.99E-99", CobolType.EXTERNAL_FLOATING_ITEM));
        assertEquals(8, CStringBinding.calcStringByteLength("PPP009999CR", CobolType.NUMERIC_EDITED_ITEM));
        assertEquals(16, CStringBinding.calcStringByteLength("$$$9999/,99.999+", CobolType.NUMERIC_EDITED_ITEM));
        assertEquals(10, CStringBinding.calcStringByteLength("****999.99", CobolType.NUMERIC_EDITED_ITEM));
        assertEquals(11, CStringBinding.calcStringByteLength("-ZZZZ99.999", CobolType.NUMERIC_EDITED_ITEM));
    }

    /**
     * National case.
     */
    public void testCNationalBinding() {
        assertEquals(18, CNationalBinding.calcNationalByteLength("N(9)"));
    }

    /**
     * Dbcs case.
     */
    public void testCDbcsBinding() {
        assertEquals(8, CDbcsBinding.calcDbcsByteLength("G(4)"));
    }

    /**
     * Octet stream case.
     */
    public void testCOctetStreamBinding() {
        assertEquals(4, COctetStreamBinding.calcOctetStreamByteLength("X(4)", "DISPLAY"));
        assertEquals(4, COctetStreamBinding.calcOctetStreamByteLength("", "INDEX"));
        assertEquals(4, COctetStreamBinding.calcOctetStreamByteLength("", "POINTER"));
        assertEquals(8, COctetStreamBinding.calcOctetStreamByteLength("", "PROCEDURE-POINTER"));
        assertEquals(4, COctetStreamBinding.calcOctetStreamByteLength("", "FUNCTION-POINTER"));
    }

    /**
     * Packed decimal case.
     */
    public void testCPackedDecimalBinding() {
        assertEquals(9, CPackedDecimalBinding.calcPackedDecimalByteLength(17));
    }

    /**
     * Zoned decimal case.
     */
    public void testCZonedDecimalBinding() {
        assertEquals(14, CZonedDecimalBinding.calcZonedDecimalByteLength(14, false));
    }

    /**
     * Binary  case.
     */
    public void testCBinaryBinding() {
        assertEquals(4, CBinaryBinding.calcBinaryByteLength(9));
        assertEquals(2, CBinaryBinding.calcBinaryByteLength(4));
    }

    /**
     * Single float case.
     */
    public void testCFloatBinding() {
        assertEquals(4, CFloatBinding.calcFloatByteLength());
    }

    /**
     * Double float case.
     */
    public void testCDoubleBinding() {
        assertEquals(8, CDoubleBinding.calcDoubleByteLength());
    }
}
