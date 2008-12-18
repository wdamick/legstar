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



import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.redsimpt.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal redsimpt.
 *
 */
public class UnmarshalRedsimptTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testRedsimpt() throws Exception {

        String hexString = "c1c2c3c4c5c6c7c8c9d1d2d3d4d5d6404040";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redsimpt");

        assertEquals("ABCDEFGHIJKLMNO", dfhcommarea.getCDefinition1());
    }

    /**
     * Unmarshal java data object and test host data result.
     * Alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedsimptSecondChoice() throws Exception {

        String hexString = "f0f0f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redsimpt");

        assertEquals(123456789012345L, dfhcommarea.getCDefinition2().longValue());
    }
}
