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
import com.legstar.test.coxb.redopera.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal redopera.
 *
 */
public class UnmarshalRedoperaTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testRedoperaStringMethod() throws Exception {

        String hexString = 
          "a2a399899587d485a3889684404040404040"
        + "c1c2d1c1c4c8c1d6e4c1e9404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redopera");

        assertEquals("ABJADHAOUAZ", dfhcommarea.getFiller25().getCString());
    }

    /**
     * Unmarshal java data object and test host data result.
     * alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedoperaIntMethod() throws Exception {

        String hexString =
          "8995a3d485a3889684404040404040404040"
        + "f0f0f0f0f0f3f4f5"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "404040404040404040404040";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redopera");

        assertEquals(null, dfhcommarea.getFiller25());
        assertEquals(345, dfhcommarea.getFiller28().getCInteger());
    }
}
