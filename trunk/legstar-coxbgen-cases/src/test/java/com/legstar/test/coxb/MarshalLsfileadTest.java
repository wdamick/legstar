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

import com.legstar.test.coxb.lsfilead.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal lsfilead.
 *
 */
public class MarshalLsfileadTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "lsfilead";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testLsfilead() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
        dfhcommarea.setComNumber(100L);
        dfhcommarea.setComName("TOTO");
        dfhcommarea.setComAddress("LABAS STREET");
        dfhcommarea.setComPhone("88993314");
        dfhcommarea.setComDate("100458");
        dfhcommarea.setComAmount("00100.35");
        dfhcommarea.setComComment("A VOIR");

        assertEquals("f0f0f0f1f0f0"
        + "e3d6e3d640404040404040404040404040404040"
        + "d3c1c2c1e240e2e3d9c5c5e34040404040404040"
        + "f8f8f9f9f3f3f1f4"
        + "f1f0f0f4f5f84040"
        + "f0f0f1f0f04bf3f5"
        + "c140e5d6c9d9404040",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 79));
    }
}
