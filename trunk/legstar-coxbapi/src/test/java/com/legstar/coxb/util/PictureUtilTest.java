package com.legstar.coxb.util;


import junit.framework.TestCase;

/**
 * Test the PictureUtil class.
 *
 */
public class PictureUtilTest extends TestCase {
    
    
    /**
     * Prepare a picture clause.
     */
    public void testPreparePicture() {
        assertEquals("X(15)", PictureUtil.preparePicture(" x( 1 5 ) "));
    }
    
    /**
     * Extracts the numeric value between parentheses.
     */
    public void testGetNumericInParentheses() {
        assertEquals(5, PictureUtil.getNumericInParentheses("  (5)  "));
        assertEquals(77, PictureUtil.getNumericInParentheses("( 77 )  "));
        assertEquals(1, PictureUtil.getNumericInParentheses("   (1)"));
    }

    /**
     * Extracts the number of symbols.
     */
    public void testGetSymbolsNumber() {
        assertEquals(0, PictureUtil.getSymbolsNumber('X', ""));
        assertEquals(1, PictureUtil.getSymbolsNumber('X', "X"));
        assertEquals(2, PictureUtil.getSymbolsNumber('X', "XX"));
        assertEquals(8, PictureUtil.getSymbolsNumber('X', "X(8)"));
    }

}
