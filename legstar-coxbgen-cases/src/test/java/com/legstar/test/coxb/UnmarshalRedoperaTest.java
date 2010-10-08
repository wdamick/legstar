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
