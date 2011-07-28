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
package com.legstar.mock.client;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.RequestException;

import junit.framework.TestCase;

/**
 * Test MockLsfileae.
 *
 */
public class MockLsfileaeTest extends TestCase {
    
    /** A sample LSFILEAE request.*/
    public static final String LSFILEAE_REQUEST =
       /* 0 0 0 1 0 0 */
        "f0f0f0f1f0f0";
    
    /** A sample LSFILEAE reply.*/
    public static final String LSFILEAE_REPLY =
        /*  0 0 0 1 0 0 */
        "f0f0f0f1f0f0"
        /*  S .  D .   B O R M A N       */
        + "e24b40c44b40c2d6d9d4c1d54040404040404040"
        /*  L A B A S   S T R E E T                 */
        + "e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040"
        /*  3 2 1 5 6 7 7 8 */
        + "f3f2f1f5f6f7f7f8"
        /*  2 6   1 1   8 1  */
        + "f2f640f1f140f8f1"
        /*  $ 0 1 0 0 . 1 1  */
        + "5bf0f1f0f04bf1f1"
        /*  * * * * * * * * *  */
        + "5c5c5c5c5c5c5c5c5c";

    /**
     * Test return value.
     */
    public void testMockLsfileae() {
        try {
            LegStarMessage requestMessage = new LegStarMessage();
            requestMessage.addDataPart(new CommareaPart(HostData.toByteArray(LSFILEAE_REQUEST)));
            LegStarMessage replyMessage = MockLsfileae.getResponse(requestMessage);
            assertEquals(LSFILEAE_REPLY,
                    HostData.toHexString(replyMessage.getDataParts().get(0).getContent()));
        } catch (HeaderPartException e) {
            fail(e.toString());
        } catch (RequestException e) {
            fail(e.toString());
        }
    }

}
