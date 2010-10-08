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
import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;

import com.legstar.test.coxb.alltypes.Dfhcommarea;
import com.legstar.test.coxb.alltypes.bind.DfhcommareaHostToJavaTransformer;

/**
 * Unmarshal alltypes.
 *
 */
public class UnmarshalAlltypesTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testAlltypes() throws Exception {

        String hexString   = AlltypesCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "alltypes");
        AlltypesCases.checkJavaObject(dfhcommarea);
    }

    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformer() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(AlltypesCases.getHostBytesHex()));
        AlltypesCases.checkJavaObject(dfhcommarea);
    }
    
    /**
     * Check what happens if the host sends only partial data.
     * @throws Exception if trest fails
     */
    public void testHostToJavaTransformerFromPartialData() throws Exception {
        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(
                HostData.toByteArray(AlltypesCases.getHostBytesHex().substring(0, 40)));
        assertEquals("ABCD", dfhcommarea.getSString());
        byte[] cBinary = {0x01, 0x02, 0x00, 0x00};
        assertEquals(HostData.toHexString(cBinary), HostData.toHexString(dfhcommarea.getSBinary()));
        assertEquals((short) -932, dfhcommarea.getSShort());
        assertEquals(15, dfhcommarea.getSUshort());
        assertEquals(78906, dfhcommarea.getSInt());
        assertEquals(452, dfhcommarea.getSUint());
        /* From here on, the mainframe did not supply enough data so
         * everything gets initialized. */
        assertEquals(0, dfhcommarea.getSLong());
        assertEquals(0, dfhcommarea.getSUlong());
        assertEquals(new BigInteger("0"), dfhcommarea.getSXlong());
        assertEquals(new BigInteger("0"), dfhcommarea.getSUxlong());
        assertEquals(new BigDecimal("0.00"), dfhcommarea.getSDec());
        assertEquals(0.0f, dfhcommarea.getSFloat());
        assertEquals(0.0d, dfhcommarea.getSDouble());
        assertEquals(2, dfhcommarea.getAString().size());
        assertTrue(null == dfhcommarea.getAString().get(0));
        assertTrue(null == dfhcommarea.getAString().get(1));
        assertEquals(2, dfhcommarea.getAShort().size());
        assertEquals(0, (int) dfhcommarea.getAShort().get(0));
        assertEquals(0, (int) dfhcommarea.getAShort().get(1));

    }
}
