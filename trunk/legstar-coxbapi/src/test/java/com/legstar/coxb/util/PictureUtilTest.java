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
        assertEquals(2, PictureUtil.getSymbolsNumber('9', "9(2)"));
    }

}
