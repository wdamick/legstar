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
public class CultureinfoTest extends AbstractProxyHttpClientTester {

    /**
     * Create the test case.
     */
    public CultureinfoTest() {
        super("cultureinfo", HostData.toByteArray(CultureinfoCases.getHostBytesHexRequestFr()));
    }

    /** {@inheritDoc} */
    public void check(final byte[] replyBytes) {
        CultureinfoCases.checkHostBytesReplyFr(replyBytes);
    }
}
