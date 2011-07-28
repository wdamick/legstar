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
package com.legstar.test.cixs;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.CultureinfoCases;

/**
 * Test the generated cultureinfo proxy.
 * 
 */
public class CultureinfoProxyITCase extends AbstractProxyHttpClientTester {

    /**
     * Create the test case.
     */
    public CultureinfoProxyITCase() {
        super("cultureinfo", HostData.toByteArray(CultureinfoCases
                .getHostBytesHexRequestFr()));
    }

    /** {@inheritDoc} */
    public void check(final byte[] replyBytes) {
        String result = HostData.toHexString(replyBytes);
        assertEquals(
                "9f40404040404040404040404040404040404040404040404040404040404040"
                        + "c699819583854040404040404040404040404040404040404040404040404040"
                        + "86998195e08189a2404040404040404040404040404040404040404040404040",
                result.substring(0, 192));
        assertEquals(
                "f2f7f56bf3f64040404040404040404040404040404040404040404040404040"
                        + "869960c6d9404040404040404040404040404040404040404040404040404040"
                        + "c699819583854040404040404040404040404040404040404040404040404040"
                        + "86998195e08189a2404040404040404040404040404040404040404040404040",
                result.substring(256, 512));
    }

    /**
     * Order is important to avoid class loading issues.
     * 
     * @return
     */
    public String[] getDeployables() {
        return new String[] { "target/war/legstar-test-cultureinfo.war",
                "target/war/c2ws-cultureinfo.war" };
    }

}
