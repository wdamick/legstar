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
import com.legstar.test.coxb.vararcom.bind.DfhcommareaHostToJavaTransformer;
import com.legstar.test.coxb.vararcom.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal varacom.
 *
 */
public class UnmarshalVararcomTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testVararcom() throws Exception {

        String hexString   = "0000";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "vararcom");

        assertEquals(0, dfhcommarea.getCItemsNumber());
    }

    /**
     * Transform host data and test java data object result.
     * Empty case.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerEmpty() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(VararcomCases.getHostBytesHexEmpty()));
        VararcomCases.checkJavaObjectEmpty(dfhcommarea);
    }
    /**
     * Unmarshal host data and test java data object result.
     * Size 10 case.
     * @throws Exception if marshaling fails
     */
    public void testVararcomSize10() throws Exception {

        String hexString   = VararcomCases.getHostBytesHexSome();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "vararcom");
        VararcomCases.checkJavaObjectSome(dfhcommarea);
    }

    /**
     * Transform host data and test java data object result.
     * 10 items.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerSome() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(VararcomCases.getHostBytesHexSome()));
        VararcomCases.checkJavaObjectSome(dfhcommarea);
    }

    /**
     * Transform host data and test java data object result.
     * 250 items.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerFull() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(VararcomCases.getHostBytesHexFull()));
        VararcomCases.checkJavaObjectFull(dfhcommarea);
    }
}
