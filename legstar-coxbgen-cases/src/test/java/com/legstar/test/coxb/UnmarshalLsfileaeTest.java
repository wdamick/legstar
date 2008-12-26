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
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaHostToJavaTransformer;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal lsfileae.
 *
 */
public class UnmarshalLsfileaeTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testLsfileae() throws Exception {

        String hexString = LsfileaeCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "lsfileae");
        LsfileaeCases.checkJavaObject(dfhcommarea);
    }
    /**
     * Transform host data and test java data object result.
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformer() throws HostTransformException {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(LsfileaeCases.getHostBytesHex()));
        LsfileaeCases.checkJavaObject(dfhcommarea);
    }
    
    /**
     * Test the sample code shown in documentation.
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformerDoc() throws HostTransformException {

        hostToJavaTransform(HostData.toByteArray(LsfileaeCases.getHostBytesHex()));
    }
    /**
     * Transform host data and test java data object result.
     * @param hostBytes a byte array holding the mainframe payload
     * @throws HostTransformException if transforming fails
     */
    public void hostToJavaTransform(final byte[] hostBytes) throws HostTransformException {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(hostBytes);
        System.out.println(dfhcommarea.getComNumber());
        System.out.println(dfhcommarea.getComPersonal().getComName());
        System.out.println(dfhcommarea.getComPersonal().getComAddress());
        System.out.println(dfhcommarea.getComPersonal().getComPhone());
        System.out.println(dfhcommarea.getComDate());
        System.out.println(dfhcommarea.getComAmount());
        System.out.println(dfhcommarea.getComComment());
    }
}
