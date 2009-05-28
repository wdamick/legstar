/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.cixs;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.CultureinfoCases;

/**
 * Test the generated cultureinfo proxy.
 *
 */
public class CultureinfoTest extends AbstractHttpClientTester {

    /**
     * Assuming the servlet has been deployed. Test remotely.
     */
    public void testProxyInvokeRemote() {
        try {
            byte[] bytesReply = postBytes(
                    "http://megamouss:8080/c2ws-cultureinfo/cultureinfoProxy",
                    HostData.toByteArray(CultureinfoCases.getHostBytesHexRequestFr()));
            CultureinfoCases.checkHostBytesReplyFr(bytesReply);
        } catch (Exception e) {
            fail(e.getMessage());
        }

    }
}
