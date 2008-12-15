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

import com.legstar.test.coxb.redmulti.Dfhcommarea;
import com.legstar.test.coxb.redmulti.Filler35;
import com.legstar.test.coxb.redmulti.Filler38;

import junit.framework.TestCase;

public class MarshalRedmultiTest extends TestCase {

    private final static String SCHEMA_NAME = "redmulti";

    public void testRedmultiNormal() throws Exception{
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        Dfhcommarea.setCOutputType("normal");
        Filler35 filler35 = new Filler35();
        filler35.setCString("ABJADHAOUAZ");
        Dfhcommarea.setFiller35(filler35);

        assertEquals("959699948193c1c2d1c1c4c8c1d6e4c1e9404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040",
                Util.marshal(SCHEMA_NAME, Dfhcommarea, 206));
    }

    public void testRedmultiError() throws Exception{
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        Dfhcommarea.setCOutputType("error");
        Filler38 filler38 = new Filler38();
        filler38.setCErrorNum(75);
        filler38.setCErrorDescription("ABOMINABLE");
        Dfhcommarea.setFiller38(filler38);

        assertEquals("859999969940f0f0f7f5c1c2d6d4c9d5c1c2d3c5404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040",
                Util.marshal(SCHEMA_NAME, Dfhcommarea, 206));
    }

}
