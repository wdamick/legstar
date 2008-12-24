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
import com.legstar.test.coxb.redopera.bind.DfhcommareaHostToJavaTransformer;

import junit.framework.TestCase;

/**
 * Unmarshal redopera.
 *
 */
public class UnmarshalRedoperaTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testRedoperaStringMethod() throws Exception {

        String hexString = RedoperaCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redopera");
        RedoperaCases.checkJavaObject(dfhcommarea);
    }

    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerStringMethod() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(RedoperaCases.getHostBytesHex()));
        RedoperaCases.checkJavaObject(dfhcommarea);
    }
    /**
     * Unmarshal host data and test java data object result.
     * alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedoperaIntMethod() throws Exception {

        String hexString = RedoperaCases.getHostBytesHexIntMethod();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redopera");
        RedoperaCases.checkJavaObjectIntMethod(dfhcommarea);
    }
    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerIntMethod() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(RedoperaCases.getHostBytesHexIntMethod()));
        RedoperaCases.checkJavaObjectIntMethod(dfhcommarea);
    }
}
