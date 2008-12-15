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

import com.legstar.test.coxb.redbotha.Filler22;
import com.legstar.test.coxb.redbotha.Dfhcommarea;

import junit.framework.TestCase;

public class MarshalRedbothaTest extends TestCase {

    private final static String SCHEMA_NAME = "redbotha";

    public void testRedbotha() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        Dfhcommarea.setCNumeric(5);

        assertEquals("0005",
                Util.marshal(SCHEMA_NAME, Dfhcommarea, 2));
    }

    public void testRedbothaSecondChoice() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        Filler22 filler22 = new Filler22();
        filler22.setCLeftByte("A");
        filler22.setCRightByte("B");
        Dfhcommarea.setFiller22(filler22);

        assertEquals("c1c2",
                Util.marshal(SCHEMA_NAME, Dfhcommarea, 2));
    }
}
