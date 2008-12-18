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
import com.legstar.test.coxb.vararcom.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal varacom.
 *
 */
public class UnmarshalVararcomTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testVararcom() throws Exception {

        String hexString   = "0000";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "vararcom");

        assertEquals(0, dfhcommarea.getCItemsNumber());
    }

    /**
     * Unmarshal java data object and test host data result.
     * Size 10 case.
     * @throws Exception if marshaling fails
     */
    public void testVararcomSize10() throws Exception {

        String hexString   = "000a"
        + "c1c2d1c1c40000"
        + "c1c2d1c1c40007"
        + "c1c2d1c1c4000e"
        + "c1c2d1c1c40015"
        + "c1c2d1c1c4001c"
        + "c1c2d1c1c40023"
        + "c1c2d1c1c4002a"
        + "c1c2d1c1c40031"
        + "c1c2d1c1c40038"
        + "c1c2d1c1c4003f";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "vararcom");

        assertEquals(10, dfhcommarea.getCItemsNumber());
        for (int i = 0; i < 10; i++) {
            com.legstar.test.coxb.vararcom.CArray item = dfhcommarea.getCArray().get(i);
            assertEquals("ABJAD", item.getCItem1());
            assertEquals(Short.parseShort(Integer.toString(7 * i)), item.getCItem2());
        }
    }
}
