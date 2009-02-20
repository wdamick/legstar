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
package com.legstar.coxb.convert.simple.test;


import com.legstar.coxb.host.HostException;
import com.legstar.util.*;


import junit.framework.TestCase;

/** Program under test has a main method because it needs to be
 * invokable from ant. */
public class ByteLengthTest extends TestCase {

    public StringBuffer outContent;


    public void testLsfileae() throws HostException {
        assertEquals("79",JaxbUtil.byteLength("com.legstar.test.coxb.lsfileae","Dfhcommarea"));
    }

    public void testFixarsim() throws HostException {

        assertEquals("15", JaxbUtil.byteLength("com.legstar.test.coxb.fixarsim","Dfhcommarea"));
    }

    public void testFixarnum() throws HostException {

        assertEquals("78", JaxbUtil.byteLength("com.legstar.test.coxb.fixarnum","Dfhcommarea"));
    }

    public void testFixarcom() throws HostException {

        assertEquals("49", JaxbUtil.byteLength("com.legstar.test.coxb.fixarcom","Dfhcommarea"));
    }

    public void testVararcom() throws HostException {

        assertEquals("1752", JaxbUtil.byteLength("com.legstar.test.coxb.vararcom","Dfhcommarea"));
    }

    public void testBinnatsi() throws HostException {

        assertEquals("56", JaxbUtil.byteLength("com.legstar.test.coxb.binnatsi","Dfhcommarea"));
    }

    public void testBinnatus() throws HostException {

        assertEquals("56", JaxbUtil.byteLength("com.legstar.test.coxb.binnatus","Dfhcommarea"));
    }

    public void testBinnpkdus() throws HostException {

        assertEquals("44", JaxbUtil.byteLength("com.legstar.test.coxb.binpkdus","Dfhcommarea"));
    }

    public void testDplarcht() throws HostException {

        assertEquals("32025", JaxbUtil.byteLength("com.legstar.test.coxb.dplarcht","Dfhcommarea"));
    }


    public void testLsfilead() throws HostException {

        assertEquals("79", JaxbUtil.byteLength("com.legstar.test.coxb.lsfilead","Dfhcommarea"));
    }

    public void testRedbotha() throws HostException {

        assertEquals("2", JaxbUtil.byteLength("com.legstar.test.coxb.redbotha","Dfhcommarea"));
    }

    public void testRedinout() throws HostException {

        assertEquals("502", JaxbUtil.byteLength("com.legstar.test.coxb.redinout","Dfhcommarea"));
    }

    public void testRedmulti() throws HostException {

        assertEquals("206", JaxbUtil.byteLength("com.legstar.test.coxb.redmulti","Dfhcommarea"));
    }

    public void testRedopera() throws HostException {

        assertEquals("218", JaxbUtil.byteLength("com.legstar.test.coxb.redopera","Dfhcommarea"));
    }

    public void testRedsimpt() throws HostException {

        assertEquals("18", JaxbUtil.byteLength("com.legstar.test.coxb.redsimpt","Dfhcommarea"));
    }

    public void testsmix() throws HostException {

        assertEquals("176", JaxbUtil.byteLength("com.legstar.test.coxb.typesmix","Dfhcommarea"));
    }
}

