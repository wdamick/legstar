/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.http.client;

import java.util.HashMap;

import com.legstar.config.Constants;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.T1volumeCases;

/**
 * Get a sense of performances in various scenario.
 *
 */
public class VolumeTest extends AbstractTester {

    /** Maximum number of iterations. */
    private static final int MAX_ITERATIONS = 5;

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS23");
        getEndpoint().setHostTraceMode(false); // dont flood the host
    }

    /**
     * Single thread iterates thru large data requests.
     */
    public void testSingleIterateVolume() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUME");
            map.put(Constants.CICS_LENGTH_KEY, "32767");
            map.put(Constants.CICS_DATALEN_KEY, "32767");

            LegStarRequest request = getRequest(map);
            request.getRequestMessage().addDataPart(
                    new CommareaPart(T1volumeCases.getHostBytes(32767)));
            for (int i = 0; i < MAX_ITERATIONS; i++) {
                getConnection().sendRequest(request);
                getConnection().recvResponse(request);
                getConnection().keepUOW();
                T1volumeCases.checkByteArray(request.getResponseMessage().getDataParts().get(0).getContent());
            }
        } catch (RequestException e) {
            e.printStackTrace();
            fail("testSingleIterateVolume failed=" + e);
        }
    }
}
