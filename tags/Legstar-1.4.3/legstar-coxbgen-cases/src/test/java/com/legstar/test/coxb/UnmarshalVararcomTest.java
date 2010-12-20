/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
