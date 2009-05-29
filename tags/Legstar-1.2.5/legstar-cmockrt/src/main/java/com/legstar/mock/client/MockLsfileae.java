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
package com.legstar.mock.client;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.RequestException;

/**
 * Mocks the behavior of the LSFILEAE program.
 *
 */
public final class MockLsfileae {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(MockLsfileae.class);
    
    /** Utility class.*/
    private MockLsfileae() {
        
    }

    /**
     * Create a response to LSFILEAE execution request.
     * @param requestMessage the request message
     * @return formatted response
     * @throws RequestException if response cannot be built
     */
    public static  LegStarMessage getResponse(
            final LegStarMessage requestMessage) throws RequestException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Building response for program LSFILEAE");
        }
        try {
            byte[] dfhcommareaResponse = new MockFILEA().getCustomer(
                    requestMessage.getDataParts().get(0).getContent());
            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new CommareaPart(dfhcommareaResponse));
            return replyMessage;
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        } catch (IOException e) {
            throw new RequestException(e);
        }
    }
    
}
