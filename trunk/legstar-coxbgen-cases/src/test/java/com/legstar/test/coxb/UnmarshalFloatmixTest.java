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
import com.legstar.test.coxb.floatmix.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal floatmix.
 *
 */
public class UnmarshalFloatmixTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testFloatmix() throws Exception {

        String hexString   =
            "434d2000000000004110000045543ae9361677a460ffffff000000000000000000000000000000000000000000000000";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "floatmix");

        assertEquals(0f, dfhcommarea.getCFloat0());
        assertEquals(1f, dfhcommarea.getCFloat1());
        assertEquals(1234f, dfhcommarea.getCFloat1234());
        assertEquals(345006.56779999996f, dfhcommarea.getCFloat345006P5678());
        assertEquals(3.40282347E+38f, dfhcommarea.getCFloat3P40282347Ep38());
        assertEquals(7.982005E-14f, dfhcommarea.getCFloat798P20067Em16());
    }
}
