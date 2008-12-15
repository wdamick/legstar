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

import com.legstar.test.coxb.redinout.Dfhcommarea;
import com.legstar.test.coxb.redinout.CParain;

import junit.framework.TestCase;

public class MarshalRedinoutTest extends TestCase {

    private final static String SCHEMA_NAME = "redinout";

    public void testRedinout() throws Exception{
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        Dfhcommarea.setCNumeric(35);
        CParain parain = new CParain();
        parain.setCSomeInput("ABCDEABCDEABCDE");
        Dfhcommarea.setCParain(parain);

        assertEquals("0023c1c2c3c4c5c1c2c3c4c5c1c2c3c4c5",
                Util.marshal(SCHEMA_NAME, Dfhcommarea, 17));
    }

}
