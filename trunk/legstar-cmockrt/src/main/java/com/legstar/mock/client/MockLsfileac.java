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

import java.math.BigDecimal;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.convert.simple.CobolBinarySimpleConverter;
import com.legstar.coxb.convert.simple.CobolPackedDecimalSimpleConverter;
import com.legstar.coxb.convert.simple.CobolStringSimpleConverter;
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.RequestException;

/**
 * Mocks the behavior of the LSFILEAE program.
 *
 */
public class MockLsfileac {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(MockLsfileac.class);
    
    /** In memory representation if the FILEA content. */
    private static final MockFILEA FILEA = new MockFILEA();;
    
    /** Utility class.*/
    private MockLsfileac() {
        
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
            LOG.debug("Building response for program LSFILEAC");
        }
        try {
            byte[] queryData = null;
            byte[] queryLimit = null;
            for (LegStarMessagePart part : requestMessage.getDataParts()) {
                if (part.getPartID().equals("QueryData")) {
                    queryData = part.getContent();
                }
                if (part.getPartID().equals("QueryLimit")) {
                    queryLimit = part.getContent();
                }
            }
            String namePattern = (queryData == null) ? "*" : getQueryName(queryData);
            List < byte[] > customers;
            if (queryLimit == null) {
                customers = FILEA.getCustomers(namePattern);
            } else {
                customers = FILEA.getCustomers(namePattern, getMaxItems(queryLimit));
            }
            
            int offset = 0;
            String hostCharsetName = CobolContext.getDefaultHostCharsetName();
            byte[] hostReplyStatus = new byte[151];

            /* reply type */
            CobolBinarySimpleConverter.toHostSingle(
                    BigDecimal.ZERO, 2, false, hostReplyStatus, offset);
            offset += 2;

            /* search duration */
            CobolStringSimpleConverter.toHostSingle(
                    "00:00:00",
                    hostCharsetName, 8, false, hostReplyStatus, offset);
            offset += 8;

            /* total items */
            CobolPackedDecimalSimpleConverter.toHostSingle(
                    new BigDecimal(FILEA.getCustomersNumber()),
                    5, 8, 0, false, hostReplyStatus, offset);
            offset += 5;

            /* resp */
            CobolBinarySimpleConverter.toHostSingle(
                    BigDecimal.ZERO, 4, true, hostReplyStatus, offset);
            offset += 4;

            /* resp2 */
            CobolBinarySimpleConverter.toHostSingle(
                    BigDecimal.ZERO, 4, true, hostReplyStatus, offset);
            offset += 4;

            /* reply message */
            CobolStringSimpleConverter.toHostSingle(
                    (customers.size() > 0) ? "" : "NO CUSTOMER SATISFIES YOUR QUERY",
                    hostCharsetName, 128, false, hostReplyStatus, offset);
            offset += 128;
            
            offset = 0;
            byte[] hostReplyData = null;
            if (customers.size() > 0) {
                hostReplyData = new byte[5 + 79 * customers.size()];
                /* items number */
                CobolPackedDecimalSimpleConverter.toHostSingle(
                        new BigDecimal(customers.size()),
                        5, 8, 0, false, hostReplyData, offset);
                offset += 5;
    
                /* items */
                for (byte[] customer : customers) {
                    System.arraycopy(customer, 0, hostReplyData, offset, 79);
                    offset += 79;
                }
            }

            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new ContainerPart("ReplyStatus", hostReplyStatus));
            replyMessage.addDataPart(new ContainerPart("ReplyData", hostReplyData));
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
     * Get the max number of items from a host byte array.
     * @param hostData the host byte array
     * @return the max number of items
     */
    private static long getMaxItems(final byte[] hostData) {
        try {
            BigDecimal bigD = CobolPackedDecimalSimpleConverter.fromHostSingle(
                    5, 8, 0, hostData, 0);
            return bigD.longValue();
        } catch (CobolConversionException e) {
            e.printStackTrace();
            return 0L;
        }
    }
}
