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
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal lsfileae.
 *
 */
public class UnmarshalLsfileaeTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testLsfileae() throws Exception {

        String hexString = "f0f0f0f1f0f0"
            + "e3d6e3d640404040404040404040404040404040"
            + "d3c1c2c1e240e2e3d9c5c5e34040404040404040"
            + "f8f8f9f9f3f3f1f4"
            + "f1f0f0f4f5f84040"
            + "f0f0f1f0f04bf3f5"
            + "c140e5d6c9d9404040";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "lsfileae");

        assertEquals(100, dfhcommarea.getComNumber());
        assertEquals("TOTO", dfhcommarea.getComPersonal().getComName().trim());
        assertEquals("LABAS STREET", dfhcommarea.getComPersonal().getComAddress().trim());
        assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone().trim());
        assertEquals("100458", dfhcommarea.getComDate().trim());
        assertEquals("00100.35", dfhcommarea.getComAmount().trim());
        assertEquals("A VOIR", dfhcommarea.getComComment().trim());
    }
}
