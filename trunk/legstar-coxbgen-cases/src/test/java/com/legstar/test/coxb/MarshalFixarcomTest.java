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

import com.legstar.test.coxb.fixarcom.CArray;
import com.legstar.test.coxb.fixarcom.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal fixarcom.
 *
 */
public class MarshalFixarcomTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "fixarcom";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testFixarcom() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        for (int i = 0; i < 7; i++) {
            CArray cArray = new CArray();
            cArray.setCItem1("ABJA" + Integer.toString(i));
            cArray.setCItem2(Short.parseShort(Integer.toString(7 * i)));
            dfhcommarea.getCArray().add(cArray);
        }

        assertEquals("c1c2d1c1f00000"
        + "c1c2d1c1f10007"
        + "c1c2d1c1f2000e"
        + "c1c2d1c1f30015"
        + "c1c2d1c1f4001c"
        + "c1c2d1c1f50023"
        + "c1c2d1c1f6002a",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 49));
    }
}
