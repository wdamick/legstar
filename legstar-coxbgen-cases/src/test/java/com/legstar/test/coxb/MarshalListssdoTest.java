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

import java.util.ArrayList;
import java.util.List;

import com.legstar.test.coxb.listssdo.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal listssdo.
 *
 */
public class MarshalListssdoTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "listssdo";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testListssdo() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        List < String > listOdo = new ArrayList < String >();
        listOdo.add("ODO01");
        listOdo.add("ODO02");
        listOdo.add("ODO03");
        listOdo.add("ODO04");
        listOdo.add("ODO05");
        dfhcommarea.getListOdo().addAll(listOdo);

        assertEquals("00000005"
        + "d6c4d6f0f1"
        + "d6c4d6f0f2"
        + "d6c4d6f0f3"
        + "d6c4d6f0f4"
        + "d6c4d6f0f5",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 29));
    }
}
