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

/**
 * Marshal redmulti.
 *
 */
public class MarshalRedmultiTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "redmulti";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testRedmultiNormal() throws Exception {
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        dfhcommarea.setCOutputType("normal");
        Filler35 filler35 = new Filler35();
        filler35.setCString("ABJADHAOUAZ");
        dfhcommarea.setFiller35(filler35);

        assertEquals("959699948193"
        + "c1c2d1c1c4c8c1d6e4c1"
        + "e9404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 206));
    }

    /**
     * Marshal host data and test java data object result.
     * Alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedmultiError() throws Exception {
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        dfhcommarea.setCOutputType("error");
        Filler38 filler38 = new Filler38();
        filler38.setCErrorNum(75);
        filler38.setCErrorDescription("ABOMINABLE");
        dfhcommarea.setFiller38(filler38);

        assertEquals("859999969940"
        + "f0f0f7f5"
        + "c1c2d6d4c9d5"
        + "c1c2d3c5404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 206));
    }

}
