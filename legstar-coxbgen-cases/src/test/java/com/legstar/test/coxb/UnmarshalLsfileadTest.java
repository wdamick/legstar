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
import com.legstar.test.coxb.lsfilead.Dfhcommarea;

import junit.framework.TestCase;

public class UnmarshalLsfileadTest extends TestCase {

    public void testLsfilead() throws Exception {

        //		            <----------><--------------------------------------><--------------------------------------><--------------><--------------><--------------><---------------->
        //		            1 2 3 4 5 6 1 2 3 4 5 6 7 8 9 10111213141516171819201 2 3 4 5 6 7 8 9 10111213141516171819201 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 9
        //		            0 0 0 1 0 0 T O T O                                 L A B A S   S T R E E T                 8 8 9 9 3 3 1 4 1 0 0 4 5 8 0 0 1 0 0 . 3 5 A   V O I R
        String hexString = "f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e240e2e3d9c5c5e34040404040404040f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";
        byte[] hostBytes = HostData.toByteArray(hexString);

        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "lsfilead");

        assertEquals(100,Dfhcommarea.getComNumber());
        assertEquals("TOTO",Dfhcommarea.getComName().trim());
        assertEquals("LABAS STREET",Dfhcommarea.getComAddress().trim());
        assertEquals("88993314",Dfhcommarea.getComPhone().trim());
        assertEquals("100458",Dfhcommarea.getComDate().trim());
        assertEquals("00100.35",Dfhcommarea.getComAmount().trim());
        assertEquals("A VOIR",Dfhcommarea.getComComment().trim());
    }
}
