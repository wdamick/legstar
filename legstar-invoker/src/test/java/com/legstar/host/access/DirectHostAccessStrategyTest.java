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
package com.legstar.host.access;

import com.legstar.coxb.host.HostData;
import com.legstar.host.AbstractTester;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarRequest;
import com.legstar.test.coxb.LsfileaeCases;

/**
 * Direct (non pooled) host access.
 *
 */
public class DirectHostAccessStrategyTest extends AbstractTester {

    /**
     * Check normal loading.
     */
    public void testConstructor() {
        HostEndpoint endpoint = getStandardHostEndpoint();
        DirectHostAccessStrategy dha = new DirectHostAccessStrategy(endpoint);
        assertTrue(dha != null);
    }

    /**
     * Try invoking the mainframe.
     */
    public void testInvoke() {
        try {
            HostEndpoint endpoint = getStandardHostEndpoint();
            DirectHostAccessStrategy dha = new DirectHostAccessStrategy(endpoint);
            LegStarRequest request = createLsfileaeRequest();
            dha.invoke(request);
            assertEquals(LsfileaeCases.getHostBytesHexReply100(), 
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (HostAccessStrategyException e) {
            fail(e.getMessage());
        }
    }
}
