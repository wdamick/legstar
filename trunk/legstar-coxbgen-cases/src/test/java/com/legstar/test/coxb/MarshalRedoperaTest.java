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

import com.legstar.coxb.test.RedoperaCases;
import com.legstar.test.coxb.redopera.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal redopera.
 *
 */
public class MarshalRedoperaTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "redopera";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testRedoperaStringMethod() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedoperaCases.getJavaObject();
        assertEquals(RedoperaCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 218));
    }

    /**
     * Marshal host data and test java data object result.
     * Alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedoperaIntMethod() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedoperaCases.getJavaObjectIntMethod();
        assertEquals(RedoperaCases.getHostBytesHexIntMethod(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 218));
    }
}
