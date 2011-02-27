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
import com.legstar.test.coxb.redmulti.Dfhcommarea;
import com.legstar.test.coxb.redmulti.bind.DfhcommareaHostToJavaTransformer;

import junit.framework.TestCase;

/**
 * Unmarshal redmulti.
 *
 */
public class UnmarshalRedmultiTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testRedmultiNormal() throws Exception {
        String hexString = RedmultiCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redmulti");
        RedmultiCases.checkJavaObject(dfhcommarea);
    }

    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerNormal() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(RedmultiCases.getHostBytesHex()));
        RedmultiCases.checkJavaObject(dfhcommarea);
    }
    /**
     * Unmarshal host data and test java data object result.
     * Alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedmultiError() throws Exception {
        String hexString = RedmultiCases.getHostBytesHexError();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redmulti");
        RedmultiCases.checkJavaObjectError(dfhcommarea);
    }

    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerError() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(RedmultiCases.getHostBytesHexError()));
        RedmultiCases.checkJavaObjectError(dfhcommarea);
    }
}
