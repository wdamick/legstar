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
package com.legstar.mq.mqcih;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.mq.mqcih.bind.MqcihTransformers;

import junit.framework.TestCase;

/**
 * Test the Mqcih class.
 *
 */
public class MqcihTest extends TestCase {
    
    /**
     * Check that default mqcih gets transformed ok.
     */
    public void testMqcih() {
        try {
            MqcihTransformers xf = new MqcihTransformers();
            byte[] hostData = xf.toHost(getValueObject());
            assertEquals("c3c9c840"
                    + "00000002"
                    + "000000b4"
                    + "00000000"
                    + "00000000"
                    + "4040404040404040"
                    + "00000000"
                    + "00000000"
                    + "00000000"
                    + "00000000"
                    + "00000111"
                    + "fffffffe"
                    + "00000001"
                    + "ffffffff"
                    + "00000000"
                    + "00000000"
                    + "00000000"
                    + "00000000"
                    + "0000000000000000"
                    + "40404040"
                    + "40404040"
                    + "4040404040404040"
                    + "4040404040404040"
                    + "4040404040404040"
                    + "40404040"
                    + "40404040"
                    + "40404040"
                    + "40404040"
                    + "40404040"
                    + "40404040"
                    + "40404040"
                    + "40404040"
                    + "4040404040404040"
                    + "4040404040404040"
                    + "00000000"
                    + "00000000"
                    + "00000000"
                    + "00000000", HostData.toHexString(hostData));
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
        
    }
    
    /**
     * @return an mqcih JAXB object with default values
     */
    private Mqcih getValueObject() {
        Mqcih mqcih = new Mqcih();
        assertEquals("CIH", mqcih.getMqcihStrucid());
        assertEquals(2, mqcih.getMqcihVersion());
        assertEquals(180, mqcih.getMqcihStruclength());
        assertEquals(0, mqcih.getMqcihEncoding());
        assertEquals(273, mqcih.getMqcihUowcontrol());
        assertEquals(-2, mqcih.getMqcihGetwaitinterval());
        return mqcih;
    }

}
