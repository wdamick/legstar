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

import com.legstar.test.coxb.floatmix.Dfhcommarea;

import junit.framework.TestCase;

public class MarshalFloatmixTest extends TestCase {

    private final static String SCHEMA_NAME = "floatmix";

    public void testFloatmix() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        Dfhcommarea.setCFloat0(0f);
        Dfhcommarea.setCFloat1(1f);
        Dfhcommarea.setCFloat1234(1234f);
        Dfhcommarea.setCFloat345006P5678(345006.5678f);
        Dfhcommarea.setCFloat3P40282347Ep38(3.40282347E+38f);
        Dfhcommarea.setCFloat798P20067Em16(798.20067E-16f);

        assertEquals("434d2000000000004110000045543ae9361677a460ffffff000000000000000000000000000000000000000000000000",
                Util.marshal(SCHEMA_NAME, Dfhcommarea, 48));
    }
}
