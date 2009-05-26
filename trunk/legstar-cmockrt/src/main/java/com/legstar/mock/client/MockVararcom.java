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

import java.util.Formatter;
import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.RequestException;

/**
 * Mocks the behavior of the VARARCOM program.
 *
 */
public final class MockVararcom {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(MockVararcom.class);
    
    /** Utility class.*/
    private MockVararcom() {
        
    }

    /**
     * Create a response to VARARCOM execution request.
     * @param requestMessage the request message
     * @return formatted response
     * @throws RequestException if response cannot be built
     */
    public static  LegStarMessage getResponse(
            final LegStarMessage requestMessage) throws RequestException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Building response for program VARARCOM");
        }
        try {
            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new CommareaPart(
                    HostData.toByteArray(getHostBytesHex36())));
            return replyMessage;
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        }
    }
    
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex36() {
        
        StringBuilder sb = new StringBuilder("0024");
        for (int i = 0; i < 36; i++) {
            StringBuilder sbl = new StringBuilder();
            Formatter formatter = new Formatter(sbl, Locale.US);
            formatter.format("%04x", 5 * (i + 1));
            sb.append("c6c7c8c9d1" + sbl.toString());
        }

        return sb.toString();
    }
}
