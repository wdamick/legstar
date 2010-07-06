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
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.RequestException;

/**
 * Mocks the behavior of the LSFILEAE program.
 *
 */
public class MockLsfileal {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(MockLsfileal.class);
    
    /** In memory representation if the FILEA content. */
    private static final MockFILEA FILEA = new MockFILEA();;
    
    /** Utility class.*/
    private MockLsfileal() {
        
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
            LOG.debug("Building response for program LSFILEAL");
        }
        try {
            byte[] hostRequest = requestMessage.getDataParts().get(0).getContent();
            
            String namePattern = getRequestName(hostRequest);
            List < byte[] > customers = FILEA.getCustomers(namePattern);
            
            int offset = 0;
            String hostCharsetName = CobolContext.getDefaultHostCharsetName();
            byte[] hostReply = new byte[143 +  79 * customers.size()];
            
            /* reply type */
            CobolBinarySimpleConverter.toHostSingle(
                    BigDecimal.ZERO, 2, false, hostReply, offset);
            offset += 2;

            /* search duration */
            CobolStringSimpleConverter.toHostSingle(
                    "00:00:00",
                    hostCharsetName, 8, false, hostReply, offset);
            offset += 8;
            
            /* total items */
            CobolPackedDecimalSimpleConverter.toHostSingle(
                    new BigDecimal(FILEA.getCustomersNumber()),
                    5, 8, 0, false, hostReply, offset);
            offset += 5;

            /* filler */
            offset += 123;

            /* items number */
            CobolPackedDecimalSimpleConverter.toHostSingle(
                    new BigDecimal(customers.size()),
                    5, 8, 0, false, hostReply, offset);
            offset += 5;
            
            /* items */
            for (byte[] customer : customers) {
                System.arraycopy(customer, 0, hostReply, offset, 79);
                offset += 79;
            }

            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new CommareaPart(hostReply));
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
    private static String getRequestName(final byte[] hostData) {
        try {
            return CobolStringSimpleConverter.fromHostSingle(
                    CobolContext.getDefaultHostCharsetName(), 20, hostData, 0);
        } catch (CobolConversionException e) {
            e.printStackTrace();
            return null;
        }
    }
    
}
