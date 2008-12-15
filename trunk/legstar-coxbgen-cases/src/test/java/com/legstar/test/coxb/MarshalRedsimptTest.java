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

import com.legstar.test.coxb.redsimpt.Dfhcommarea;

import junit.framework.TestCase;

public class MarshalRedsimptTest extends TestCase {

    private final static String SCHEMA_NAME = "redsimpt";

    public void testRedsimpt() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        Dfhcommarea.setCDefinition1("ABCDEFGHIJKLMNO");

        //		     <------------------------------------>
        //		      1 2 3 4 5 6 7 8 9 101112131415161718
        //		      A B C D E F G H I J K L M N O       
        assertEquals("c1c2c3c4c5c6c7c8c9d1d2d3d4d5d6404040",
                Util.marshal(SCHEMA_NAME, Dfhcommarea, 18));
    }

    public void testRedsimptSecondChoice() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        Dfhcommarea.setCDefinition2(123456789012345l);

        //		     <------------------------------------>
        //		      1 2 3 4 5 6 7 8 9 101112131415161718
        //		      0 0 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5        
        assertEquals("f0f0f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5",
                Util.marshal(SCHEMA_NAME, Dfhcommarea, 18));
    }
}
