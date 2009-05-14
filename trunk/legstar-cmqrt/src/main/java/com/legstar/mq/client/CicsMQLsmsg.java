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
package com.legstar.mq.client;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.ibm.mq.MQC;
import com.ibm.mq.MQMessage;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostMessageFormatException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

/**
 * Implementation of MQ connection used when the mainframe runs LegStar modules
 * ready to process LegStar messages.
 * <p/>
 * The MQ messages payloads exchanged with the mainframe are serializations of
 * {@link com.legstar.messaging.LegStarMessage}.
 *
 */
public class CicsMQLsmsg extends AbstractCicsMQ  {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(CicsMQLsmsg.class);

    /**
     * Construct an instance of an MQ connection to the mainframe.
     * 
     * @param connectionID an identifier for this connection
     * @param cicsMQEndpoint MQ endpoint
     * @param connectionTimeout Maximum time (milliseconds) to wait for
     *  connection
     * @param receiveTimeout Maximum time (milliseconds) to wait for host reply
     * @throws CicsMQConnectionException if instantiation fails
     */
    public CicsMQLsmsg(
            final String connectionID,
            final CicsMQEndpoint cicsMQEndpoint,
            final int connectionTimeout,
            final int receiveTimeout) throws CicsMQConnectionException {
        super(connectionID, cicsMQEndpoint, connectionTimeout, receiveTimeout);
    }

    /**
     * Creates an MQ request message with appropriate header data.
     * A request is folded as MQ Headers and a binary payload. 
     * @param request request description
     * @return the MQ message
     * @throws RequestException if formatting of mq message fails
     */
    public MQMessage createMQRequestMessage(
            final LegStarRequest request) throws RequestException {

        MQMessage mqMessage = new MQMessage();

        try {
            /* Send no correlation ID. LegStar will correlate on Message ID. */
            mqMessage.correlationId = MQC.MQCI_NONE;

            /* In trace mode, use 16 characters from the application identity data
             * to pass a trace ID that is readable on the mainframe. */
            if (request.getAddress().isHostTraceMode()) {
                mqMessage.applicationIdData = "true " + request.getID();
            } else {
                mqMessage.applicationIdData = "false";
            }
            mqMessage.userId = getCicsMQEndpoint().getHostUserID();

            /* Finally create the mq message content */
            mqMessage.write(request.getRequestMessage().toByteArray());

        } catch (HostMessageFormatException e) {
            throw new RequestException(e);
        } catch (IOException e) {
            throw new RequestException(e);
        }

        return mqMessage;
    }

    /**
     * Creates a response message from the MQ reply back.
     * The MQ payload should contain serailization of a header part 
     * followed by any number of data parts.
     * @param mqMessage the MQ response message
     * @return a response message
     * @throws HostReceiveException if response cannot be mapped to a message
     */
    public LegStarMessage createResponseMessage(
            final MQMessage mqMessage) throws HostReceiveException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("enter createResponseMessage(hostBytes)");
        }

        try {
            byte[] hostBytes = new byte[mqMessage.getDataLength()];
            mqMessage.readFully(hostBytes);
            LegStarMessage reponseMessage;
            reponseMessage = new LegStarMessage();
            reponseMessage.fromByteArray(hostBytes, 0);
            if (LOG.isDebugEnabled()) {
                LOG.debug("response message received");
            }
            return reponseMessage;
        } catch (HeaderPartException e) {
            throw new HostReceiveException(e);
        } catch (HostMessageFormatException e) {
            throw new HostReceiveException(e);
        } catch (IOException e) {
            throw new HostReceiveException(e);
        }

    }

}
