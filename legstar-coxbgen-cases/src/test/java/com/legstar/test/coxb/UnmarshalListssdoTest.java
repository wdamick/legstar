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
import com.legstar.test.coxb.listssdo.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal listssdo.
 *
 */
public class UnmarshalListssdoTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testListssdo() throws Exception {

        String hexString   = "00000005"
            + "d6c4d6f0f1"
            + "d6c4d6f0f2"
            + "d6c4d6f0f3"
            + "d6c4d6f0f4"
            + "d6c4d6f0f5";
        byte[] hostBytes = HostData.toByteArray(hexString);

        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "listssdo");

        assertEquals(5, dfhcommarea.getListOdo().size());
        assertEquals("ODO01", dfhcommarea.getListOdo().get(0));
        assertEquals("ODO02", dfhcommarea.getListOdo().get(1));
        assertEquals("ODO03", dfhcommarea.getListOdo().get(2));
        assertEquals("ODO04", dfhcommarea.getListOdo().get(3));
        assertEquals("ODO05", dfhcommarea.getListOdo().get(4));
    }
}
