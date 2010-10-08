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
package com.legstar.test.coxb;


import junit.framework.TestCase;

/**
 * Test complex type byte length calculation.
 *
 */
public class ByteLengthTest extends TestCase {

    /**
     * Test fixarsim.
     * @throws Exception if test fails
     */
    public void testFixarsim() throws Exception {
        assertEquals(15, Util.getByteLength("fixarsim"));
    }

    /**
     * Test lsfileae.
     * @throws Exception if test fails
     */
    public void testLsfileae() throws Exception {
        assertEquals(79, Util.getByteLength("lsfileae"));
    }

    /**
     * Test redsimpt.
     * @throws Exception if test fails
     */
    public void testRedsimpt() throws Exception {
        assertEquals(18, Util.getByteLength("redsimpt"));
    }

    /**
     * Test vararcom.
     * @throws Exception if test fails
     */
    public void testVararcom() throws Exception {
        assertEquals(1752, Util.getByteLength("vararcom"));
    }

    /**
     * Test binarcht.
     * @throws Exception if test fails
     */
    public void testBinarcht() throws Exception {
        assertEquals(56, Util.getByteLength("binarcht"));
    }

    /**
     * Test binnatsi.
     * @throws Exception if test fails
     */
    public void testBinnatsi() throws Exception {
        assertEquals(56, Util.getByteLength("binnatsi"));
    }

    /**
     * Test doublmix.
     * @throws Exception if test fails
     */
    public void testDoublmix() throws Exception {
        assertEquals(48, Util.getByteLength("doublmix"));
    }

    /**
     * Test dplarcht.
     * @throws Exception if test fails
     */
    public void testDplarcht() throws Exception {
        assertEquals(32025, Util.getByteLength("dplarcht"));
    }

    /**
     * Test fixarcom.
     * @throws Exception if test fails
     */
    public void testFixarcom() throws Exception {
        assertEquals(49, Util.getByteLength("fixarcom"));
    }

    /**
     * Test fixarnum.
     * @throws Exception if test fails
     */
    public void testFixarnum() throws Exception {
        assertEquals(78, Util.getByteLength("fixarnum"));
    }

    /**
     * Test redbotha.
     * @throws Exception if test fails
     */
    public void testRedbotha() throws Exception {
        assertEquals(2, Util.getByteLength("redbotha"));
    }
    /**
     * Test redopera.
     * @throws Exception if test fails
     */
    public void testRedopera() throws Exception {
        assertEquals(218, Util.getByteLength("redopera"));
    }
    /**
     * Test typesmix.
     * @throws Exception if test fails
     */
    public void testTypesmix() throws Exception {
        assertEquals(176, Util.getByteLength("typesmix"));
    }

    /**
     * Test alltypes.
     * @throws Exception if test fails
     */
    public void testAlltypes() throws Exception {
        assertEquals(267, Util.getByteLength("alltypes"));
    }

    /**
     * Test redinout.
     * @throws Exception if test fails
     */
    public void testRedinout() throws Exception {
        assertEquals(502, Util.getByteLength("redinout"));
    }
}
