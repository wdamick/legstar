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

import com.legstar.test.coxb.fixarsim.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal fixarsim.
 *
 */
public class MarshalFixarsimTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "fixarsim";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testFixarsim() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        dfhcommarea.getCArray().add("PREMI");
        dfhcommarea.getCArray().add("DEUXI");
        dfhcommarea.getCArray().add("TROIS");

         assertEquals("d7d9c5d4c9c4c5e4e7c9e3d9d6c9e2",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 15));
    }
}
