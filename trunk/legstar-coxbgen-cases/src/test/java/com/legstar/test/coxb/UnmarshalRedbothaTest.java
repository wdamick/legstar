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
import com.legstar.test.coxb.redbotha.Dfhcommarea;
import com.legstar.test.coxb.redbotha.bind.DfhcommareaHostToJavaTransformer;

import junit.framework.TestCase;

/**
 * Unmarshal redbotha.
 *
 */
public class UnmarshalRedbothaTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testRedbothaBothChoice() throws Exception {

        String hexString   = RedbothaCases.getHostBytesHexSecondChoice();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redbotha");
        RedbothaCases.checkJavaObjectSecondChoice(dfhcommarea);
    }
    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerSecondChoice() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(
                HostData.toByteArray(RedbothaCases.getHostBytesHexSecondChoice()));
        RedbothaCases.checkJavaObjectSecondChoice(dfhcommarea);
    }
}
