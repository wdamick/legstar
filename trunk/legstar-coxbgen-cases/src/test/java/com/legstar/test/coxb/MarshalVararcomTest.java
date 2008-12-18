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

import com.legstar.test.coxb.vararcom.Dfhcommarea;
import com.legstar.test.coxb.vararcom.CArray;

import junit.framework.TestCase;

/**
 * Marshal vararcom.
 *
 */
public class MarshalVararcomTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "vararcom";

    /**
     * Marshal host data and test java data object result.
     * Empty case.
     * @throws Exception if marshaling fails
     */
    public void testVararcomEmpty() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        dfhcommarea.setCItemsNumber(Short.parseShort("0"));

        assertEquals("0000",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 2));
    }

    /**
     * Marshal host data and test java data object result.
     * Partially filled case.
     * @throws Exception if marshaling fails
     */
    public void testVararcomSome() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        dfhcommarea.setCItemsNumber(Short.parseShort("10"));
        for (int i = 0; i < 10; i++) {
            CArray item = new CArray();
            item.setCItem1("ABJAD");
            item.setCItem2(Short.parseShort(Integer.toString(7 * i)));
            dfhcommarea.getCArray().add(item);
        }

        assertEquals("000a"
        + "c1c2d1c1c40000"
        + "c1c2d1c1c40007"
        + "c1c2d1c1c4000e"
        + "c1c2d1c1c40015"
        + "c1c2d1c1c4001c"
        + "c1c2d1c1c40023"
        + "c1c2d1c1c4002a"
        + "c1c2d1c1c40031"
        + "c1c2d1c1c40038"
        + "c1c2d1c1c4003f",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 72));
    }

    /**
     * Marshal host data and test java data object result.
     * Completely filled case.
     * @throws Exception if marshaling fails
     */
    public void testVararcom() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        dfhcommarea.setCItemsNumber(Short.parseShort("250"));
        for (int i = 0; i < 250; i++) {
            CArray item = new CArray();
            item.setCItem1("ABJAD");
            item.setCItem2(Short.parseShort(Integer.toString(7 * i)));
            dfhcommarea.getCArray().add(item);
        }

        String result = Util.marshal(SCHEMA_NAME, dfhcommarea, 1752);
        assertEquals("00fa"
                + "c1c2d1c1c40000"
                + "c1c2d1c1c40007"
                + "c1c2d1c1c4000e"
                + "c1c2d1c1c40015", result.substring(0, 60));

        assertEquals("c1c2d1c1c406ba"
                + "c1c2d1c1c406c1"
                + "c1c2d1c1c406c8"
                + "c1c2d1c1c406cf", result.substring(3448, 3504));
    }
}
