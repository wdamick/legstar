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
