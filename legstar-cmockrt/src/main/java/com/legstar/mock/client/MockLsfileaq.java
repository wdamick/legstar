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

import java.math.BigDecimal;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.convert.simple.CobolBinarySimpleConverter;
import com.legstar.coxb.convert.simple.CobolPackedDecimalSimpleConverter;
import com.legstar.coxb.convert.simple.CobolStringSimpleConverter;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.RequestException;

/**
 * Mocks the behavior of the LSFILEAQ program.
 *
 */
public final class MockLsfileaq {

    /** In memory representation if the FILEA content. */
    private static final MockFILEA FILEA = new MockFILEA();

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(MockLsfileaq.class);
    
    /** Utility class.*/
    private MockLsfileaq() {
        
    }

    /**
     * Create a response to LSFILEAQ execution request.
     * @param requestMessage the request message
     * @return formatted response
     * @throws RequestException if response cannot be built
     */
    public static  LegStarMessage getResponse(
            final LegStarMessage requestMessage) throws RequestException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Building response for program LSFILEAQ");
        }
        try {
            byte[] hostRequest = requestMessage.getDataParts().get(0).getContent();
            String namePattern = getQueryName(hostRequest);
            byte[] hostMaxItems = new byte[2];
            System.arraycopy(hostRequest, 20, hostMaxItems, 0, 2);
            long maxReplies = getMaxReplies(hostMaxItems);
            List < byte[] > customers = FILEA.getCustomers(namePattern, -1, maxReplies);
            byte[] hostReplyData = new byte[hostRequest.length +  5 + 79 * customers.size()];

            int offset = 0;
            /* request is part of the reply */
            System.arraycopy(hostRequest, 0, hostReplyData, offset, hostRequest.length);
            offset += hostRequest.length;
           
            /* reply items number */
            CobolPackedDecimalSimpleConverter.toHostSingle(
                    new BigDecimal(customers.size()),
                    5, 8, 0, false, hostReplyData, offset);
            offset += 5;

            /* items */
            for (byte[] customer : customers) {
                System.arraycopy(customer, 0, hostReplyData, offset, 79);
                offset += 79;
            }

            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new CommareaPart(hostReplyData));
            return replyMessage;
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        } catch (CobolConversionException e) {
            throw new RequestException(e);
        }
    }
    
    /**
     * Get the request name pattern from a host byte array.
     * @param hostData the host byte array
     * @return the request name pattern
     */
    private static String getQueryName(final byte[] hostData) {
        try {
            return CobolStringSimpleConverter.fromHostSingle(
                    CobolContext.getDefaultHostCharsetName(), 20, hostData, 0);
        } catch (CobolConversionException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Get the max number of replies from a host byte array.
     * @param hostData the host byte array
     * @return the max number of items
     */
    private static long getMaxReplies(final byte[] hostData) {
        try {
            BigDecimal bigD = CobolBinarySimpleConverter.fromHostSingle(
                    2, true, 4, 0, hostData, 0);
            return bigD.longValue();
        } catch (CobolConversionException e) {
            e.printStackTrace();
            return 0L;
        }
    }
}
