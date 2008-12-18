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
import com.legstar.test.coxb.fixarnum.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal fixarnum.
 *
 */
public class UnmarshalFixarnumTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testFixarnum() throws Exception {

        String hexString   = "1653423f"
            + "0000150f"
            + "0018400f"
            + "f5f3f4f2f3f6"
            + "f0f4f5f0f0f7"
            + "f0f0f1f9f5f0"
            + "f9f9f9f8"
            + "f0f0f0f0"
            + "f0f1f7f8"
            + "3b99435e000a539f02315b1501b69b4ba630f34e00000001936299fe0000000002315bc0";
        byte[] hostBytes = HostData.toByteArray(hexString);

        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "fixarnum");

        assertEquals("16534.23", dfhcommarea.getCArrayPd().get(0).toString());
        assertEquals("1.50", dfhcommarea.getCArrayPd().get(1).toString());
        assertEquals("184.00", dfhcommarea.getCArrayPd().get(2).toString());

        assertEquals("534.236", dfhcommarea.getCArrayZd().get(0).toString());
        assertEquals("45.007", dfhcommarea.getCArrayZd().get(1).toString());
        assertEquals("1.95", dfhcommarea.getCArrayZd().get(2).toString());

        assertEquals("9998", dfhcommarea.getCArrayZi().get(0).toString());
        assertEquals("0", dfhcommarea.getCArrayZi().get(1).toString());
        assertEquals("178", dfhcommarea.getCArrayZi().get(2).toString());

        assertEquals("999899998", dfhcommarea.getCArrayBi().get(0).toString());
        assertEquals("676767", dfhcommarea.getCArrayBi().get(1).toString());
        assertEquals("36789013", dfhcommarea.getCArrayBi().get(2).toString());

        assertEquals("123456789012345678", dfhcommarea.getCArrayNi().get(0).toString());
        assertEquals("6767679998", dfhcommarea.getCArrayNi().get(1).toString());
        assertEquals("36789184", dfhcommarea.getCArrayNi().get(2).toString());
    }
}
