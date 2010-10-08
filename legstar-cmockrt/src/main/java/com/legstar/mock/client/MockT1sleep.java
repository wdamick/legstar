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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.RequestException;

/**
 * Mocks the behavior of the T1SLEEP program.
 *
 */
public final class MockT1sleep {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(MockT1sleep.class);

    /** Utility class.*/
    private MockT1sleep() {

    }

    /**
     * Create a response to T1SLEEP execution request.
     * @param requestMessage the request message
     * @return formatted response
     * @throws RequestException if response cannot be built
     */
    public static  LegStarMessage getResponse(
            final LegStarMessage requestMessage) throws RequestException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Building response for program T1SLEEP");
        }
        try {
            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new CommareaPart(
                    requestMessage.getDataParts().get(0).getContent()));
            Thread.sleep(4000L);
            return replyMessage;
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        } catch (InterruptedException e) {
            throw new RequestException(e);
        }
    }

}
