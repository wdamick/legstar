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
import com.legstar.xsdc.test.cases.JvmqueryCases;

/**
 * Test the generated Jvmquery proxy.
 *
 */
public class JvmqueryTest extends AbstractProxyHttpClientTester {

    /**
     * Create the test case.
     */
    public JvmqueryTest() {
        super("Jvmquery", HostData.toByteArray(JvmqueryCases.getHostBytesHexRequest()));
    }

    /** {@inheritDoc} */
    public void check(final byte[] replyBytes) {
        JvmqueryCases.checkHostBytesHexReplyFrance(HostData.toHexString(replyBytes));
    }
    
}
