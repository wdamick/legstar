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
package com.legstar.mq.client;

import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.Message;

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
public class CicsMQLsmsg extends AbstractCicsMQ {

    /**
     * Construct an instance of an MQ connection to the mainframe.
     * 
     * @param connectionID an identifier for this connection
     * @param cicsMQEndpoint MQ endpoint
     * @throws CicsMQConnectionException if instantiation fails
     */
    public CicsMQLsmsg(final String connectionID,
            final CicsMQEndpoint cicsMQEndpoint)
            throws CicsMQConnectionException {
        super(connectionID, cicsMQEndpoint);
    }

    /**
     * Creates an JMS/MQ request message with appropriate header data. A request
     * is folded as JMS Headers and a binary payload.
     * 
     * @param request request description
     * @return the JMS/MQ message
     * @throws RequestException if formatting of mq message fails
     */
    public Message createRequestMessage(final LegStarRequest request)
            throws RequestException {

        try {
            BytesMessage message = getJmsQueueSession().createBytesMessage();
            /* Send no correlation ID. LegStar will correlate on Message ID. */
            message.setJMSCorrelationID(null);
            /*
             * In trace mode, use 16 characters from the application identity
             * data to pass a trace ID that is readable on the mainframe.
             */
            if (request.getAddress().isHostTraceMode()) {
                message.setStringProperty("JMSXAppID",
                        "true " + request.getID());
            } else {
                message.setStringProperty("JMSXAppID", "false ");
            }
            message.setStringProperty("JMSXUserID", getHostUserID(request));

            message.writeBytes(request.getRequestMessage().toByteArray());

            return message;
        } catch (JMSException e) {
            throw new RequestException(e);
        } catch (HostMessageFormatException e) {
            throw new RequestException(e);
        }
    }

    /**
     * Creates a response message from the MQ reply back. The MQ payload should
     * contain serialization of a header part followed by any number of data
     * parts.
     * 
     * @param jmsMessage the MQ response message
     * @return a response message
     * @throws HostReceiveException if response cannot be mapped to a message
     */
    public LegStarMessage createReplyMessage(final BytesMessage jmsMessage,
            final int dataLength) throws HostReceiveException {

        try {
            byte[] hostBytes = new byte[dataLength];
            if (dataLength > 0) {
                jmsMessage.readBytes(hostBytes);
            }
            LegStarMessage replyMessage;
            replyMessage = new LegStarMessage();
            replyMessage.fromByteArray(hostBytes, 0);
            return replyMessage;
        } catch (HeaderPartException e) {
            throw new HostReceiveException(e);
        } catch (HostMessageFormatException e) {
            throw new HostReceiveException(e);
        } catch (JMSException e) {
            throw new HostReceiveException(e);
        }

    }

}
