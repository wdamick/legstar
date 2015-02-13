/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.transform;

import junit.framework.TestCase;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;


/**
 * This class is useful for performance testing with JMeter.
 *
 */
public class LsfileaeMeteringTest extends TestCase {
    
    /** A byte array holding raw mainframe data. */
    private static final byte[] LSFILEAE_HOST_BYTES =
        HostData.toByteArray(LsfileaeCases.getHostBytesHex());
    
    /**
     * LSFILEAE from Host to Java.
     */
    public void testHostToJava() {
        try {
            LsfileaeTransformers transformers = new LsfileaeTransformers();
            Dfhcommarea dfhcommarea = (Dfhcommarea) transformers.toJava(
                    LSFILEAE_HOST_BYTES);
            assertEquals("TOTO", dfhcommarea.getComPersonal().getComName().trim());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * LSFILEAE from Java to Host.
     */
    public void testJavaToHost() {
        try {
            LsfileaeTransformers transformers = new LsfileaeTransformers();
            byte[] hostBytes = transformers.toHost(
                    LsfileaeCases.getJavaObject());
            assertEquals(LSFILEAE_HOST_BYTES[0], hostBytes[0]);
            assertEquals(LSFILEAE_HOST_BYTES[LSFILEAE_HOST_BYTES.length - 1],
                    hostBytes[LSFILEAE_HOST_BYTES.length - 1]);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}
