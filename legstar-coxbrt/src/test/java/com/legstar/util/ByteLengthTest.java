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
package com.legstar.util;


import com.legstar.coxb.host.HostException;


import junit.framework.TestCase;

/**
 * Test utility that calculates the host byte length of a JAXB class.
 */
public class ByteLengthTest extends TestCase {


    /**
     * LSFILEAE case.
     * @throws HostException if calculation fails
     */
    public void testLsfileae() throws HostException {
        assertEquals("79", JaxbUtil.byteLength("com.legstar.test.coxb.lsfileae", "Dfhcommarea"));
    }

    /**
     * FIXARSIM case.
     * @throws HostException if calculation fails
     */
    public void testFixarsim() throws HostException {
        assertEquals("15", JaxbUtil.byteLength("com.legstar.test.coxb.fixarsim", "Dfhcommarea"));
    }

    /**
     * FIXARNUM case.
     * @throws HostException if calculation fails
     */
    public void testFixarnum() throws HostException {
        assertEquals("78", JaxbUtil.byteLength("com.legstar.test.coxb.fixarnum", "Dfhcommarea"));
    }

    /**
     * FIXARCOM case.
     * @throws HostException if calculation fails
     */
    public void testFixarcom() throws HostException {
        assertEquals("49", JaxbUtil.byteLength("com.legstar.test.coxb.fixarcom", "Dfhcommarea"));
    }

    /**
     * VARACOM case.
     * @throws HostException if calculation fails
     */
    public void testVararcom() throws HostException {
        assertEquals("1752", JaxbUtil.byteLength("com.legstar.test.coxb.vararcom", "Dfhcommarea"));
    }

    /**
     * BINNATSI case.
     * @throws HostException if calculation fails
     */
    public void testBinnatsi() throws HostException {
        assertEquals("56", JaxbUtil.byteLength("com.legstar.test.coxb.binnatsi", "Dfhcommarea"));
    }

    /**
     * BINNATUS case.
     * @throws HostException if calculation fails
     */
    public void testBinnatus() throws HostException {
        assertEquals("56", JaxbUtil.byteLength("com.legstar.test.coxb.binnatus", "Dfhcommarea"));
    }

    /**
     * BINPKDUS case.
     * @throws HostException if calculation fails
     */
    public void testBinnpkdus() throws HostException {
        assertEquals("44", JaxbUtil.byteLength("com.legstar.test.coxb.binpkdus", "Dfhcommarea"));
    }

    /**
     * DPLARCHT case.
     * @throws HostException if calculation fails
     */
    public void testDplarcht() throws HostException {
        assertEquals("32025", JaxbUtil.byteLength("com.legstar.test.coxb.dplarcht", "Dfhcommarea"));
    }


    /**
     * LSFILEAD case.
     * @throws HostException if calculation fails
     */
    public void testLsfilead() throws HostException {
        assertEquals("79", JaxbUtil.byteLength("com.legstar.test.coxb.lsfilead", "Dfhcommarea"));
    }

    /**
     * REDBOTHA case.
     * @throws HostException if calculation fails
     */
    public void testRedbotha() throws HostException {
        assertEquals("2", JaxbUtil.byteLength("com.legstar.test.coxb.redbotha", "Dfhcommarea"));
    }

    /**
     * REDINOUT case.
     * @throws HostException if calculation fails
     */
    public void testRedinout() throws HostException {
        assertEquals("502", JaxbUtil.byteLength("com.legstar.test.coxb.redinout", "Dfhcommarea"));
    }

    /**
     * REDMULTI case.
     * @throws HostException if calculation fails
     */
    public void testRedmulti() throws HostException {
        assertEquals("206", JaxbUtil.byteLength("com.legstar.test.coxb.redmulti", "Dfhcommarea"));
    }

    /**
     * REDOPERA case.
     * @throws HostException if calculation fails
     */
    public void testRedopera() throws HostException {
        assertEquals("218", JaxbUtil.byteLength("com.legstar.test.coxb.redopera", "Dfhcommarea"));
    }

    /**
     * REDSIMPT case.
     * @throws HostException if calculation fails
     */
    public void testRedsimpt() throws HostException {
        assertEquals("18", JaxbUtil.byteLength("com.legstar.test.coxb.redsimpt", "Dfhcommarea"));
    }

    /**
     * TESTSMIX case.
     * @throws HostException if calculation fails
     */
    public void testsmix() throws HostException {
        assertEquals("176", JaxbUtil.byteLength("com.legstar.test.coxb.typesmix", "Dfhcommarea"));
    }

    /**
     * FLOATMIX case.
     * @throws HostException if calculation fails
     */
    public void testFloatmix() throws HostException {
        assertEquals("24", JaxbUtil.byteLength("com.legstar.test.coxb.floatmix", "Dfhcommarea"));
    }
}

