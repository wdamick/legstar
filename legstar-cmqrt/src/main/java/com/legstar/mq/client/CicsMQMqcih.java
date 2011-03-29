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

import java.io.IOException;

import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.Message;

import org.json.JSONException;
import org.json.JSONObject;

import com.legstar.config.Constants;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.mq.mqcih.Mqcih;
import com.legstar.mq.mqcih.bind.MqcihTransformers;

/**
 * Implementation of MQ connection used when the mainframe runs IBM CICS MQ
 * Bridge.
 * <p/>
 * The MQ messages payloads exchanged with the mainframe starts with an MQCIH
 * header followed by the program name and finally the raw data.
 * 
 */
public class CicsMQMqcih extends AbstractCicsMQ {

    /** COBOL Binding transformer for MQCIH. */
    private MqcihTransformers _MqcihTransformers;

    /** While we find this constant somewhere in MQ. */
    private static final int MQCIH_STRUCLENGTH = 180;

    /** Flag indicating that reply should not include trailing low-values. */
    private static final int MQCIH_REPLY_WITHOUT_NULLS = 2;

    /** Byte length for a CICS program name. */
    private static final int CICS_PROGRAM_NAME_LEN = 8;

    /** Used for non Bridge messages. */
    public static final String MQFMT_NONE = "        ";

    /** Used for Bridge messages. */
    public static final String MQFMT_CICS = "MQCICS  ";

    /** Signals a new UOW to the Bridge. */
    public static final String MQNEW_SESSION = "AMQ!NEW_SESSION_CORRELID";

    /**
     * Flag indicating that a syncpoint should be taken upon return from
     * execution of DPL program. TODO revise when we implement single phase
     * commit
     */
    private static final int MQCIH_SYNC_ON_RETURN = 4;

    /**
     * Means that the DPL interaction is the only one with the UOW.. TODO revise
     * when we implement single phase commit
     */
    private static final int MQCUOWC_ONLY = 273;

    /**
     * Construct an instance of an MQ connection to the mainframe.
     * 
     * @param connectionID an identifier for this connection
     * @param cicsMQEndpoint MQ endpoint
     * @throws CicsMQConnectionException if instantiation fails
     */
    public CicsMQMqcih(final String connectionID,
            final CicsMQEndpoint cicsMQEndpoint)
            throws CicsMQConnectionException {
        super(connectionID, cicsMQEndpoint);
        _MqcihTransformers = new MqcihTransformers();
    }

    /**
     * Creates a JMS/MQ request message with appropriate header data. A request
     * is folded as JMS Headers and a binary payload.
     * <p/>
     * The IBM CICS MQ Bridge cannot support channel/containers.
     * 
     * @param request request description
     * @return the MQ message
     * @throws RequestException if formatting of mq message fails
     */
    public Message createRequestMessage(final LegStarRequest request)
            throws RequestException {

        if (request.getRequestMessage().getDataParts().size() > 1) {
            throw new RequestException(
                    "IBM CICS MQ Bridge does not support channel/containers");
        }

        String hostCharset = getHostCharset(request);
        byte[] hostData = new byte[0];
        if (request.getRequestMessage().getDataParts().size() > 0) {
            hostData = request.getRequestMessage().getDataParts().get(0)
                    .getContent();
        }

        try {
            BytesMessage message = getJmsQueueSession().createBytesMessage();
            message.setJMSMessageID(null);
            /*
             * CKBR uses the correlationID for UOW lifecycle management. There
             * is no support for multiple message UOW. We assume this is the
             * start of new UOW.
             */
            message.setJMSCorrelationID(MQNEW_SESSION);

            /*
             * Tells MQ-mainframe that the content is an MQCIH payload and that
             * it is already encoded in mainframe character set.
             */
            message.setStringProperty("JMS_IBM_Format", MQFMT_CICS);
            message.setIntProperty("JMS_IBM_Character_Set",
                    getCCSID(hostCharset));

            message.setStringProperty("JMSXUserID", getHostUserID(request));

            /* Get parameters from the LegStar header */
            JSONObject jsonObj = new JSONObject(request.getRequestMessage()
                    .getHeaderPart().getJsonString());
            String programName = (String) jsonObj
                    .get(Constants.CICS_PROGRAM_NAME_KEY);

            /*
             * The output data length must account for the 8 characters program
             * name
             */
            int outputdatalength = jsonObj.getInt(Constants.CICS_LENGTH_KEY) + 8;

            /* Finally create the mq message content: MQCIH + PROGRAM + COMMAREA */
            byte[] mqcihBytes = getMqcihTransformers().toHost(
                    getMqcih(outputdatalength, getHostPassword(request)),
                    hostCharset);
            byte[] result = new byte[mqcihBytes.length + CICS_PROGRAM_NAME_LEN
                    + hostData.length];
            System.arraycopy(mqcihBytes, 0, result, 0, mqcihBytes.length);

            /* Add program name (pad with spaces if too short) */
            byte[] programBytes = "        ".getBytes(hostCharset);
            System.arraycopy(programName.getBytes(hostCharset), 0,
                    programBytes, 0, programName.length());
            System.arraycopy(programBytes, 0, result, mqcihBytes.length,
                    CICS_PROGRAM_NAME_LEN);

            System.arraycopy(hostData, 0, result, mqcihBytes.length
                    + CICS_PROGRAM_NAME_LEN, hostData.length);
            message.writeBytes(result);

            return message;

        } catch (IOException e) {
            throw new RequestException(e);
        } catch (JMSException e) {
            throw new RequestException(e);
        } catch (JSONException e) {
            throw new RequestException(e);
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        } catch (HostTransformException e) {
            throw new RequestException(e);
        }

    }

    /**
     * Creates a response message from the JMS/MQ reply back. The JMS payload
     * should start with an MQCIH header followed by the program name and the
     * raw data.
     * 
     * @param jmsMessage the JMS response message
     * @param dataLength the data length
     * @return a response message
     * @throws HostReceiveException if response cannot be mapped to a message
     */
    public LegStarMessage createReplyMessage(final BytesMessage jmsMessage,
            final int dataLength) throws HostReceiveException {

        try {
            /* Check that reply has an MQCIH header */
            String format = jmsMessage.getStringProperty("JMS_IBM_Format");
            String hostCharset = getHostCharset(jmsMessage
                    .getIntProperty("JMS_IBM_Character_Set"));

            if (format == null || !format.equals(MQFMT_CICS)) {
                throw new HostReceiveException(
                        "Reply does not hold an MQCIH header");
            }

            if (dataLength < MQCIH_STRUCLENGTH) {
                throw new HostReceiveException(
                        "Reply too small to hold a MQCIH header");
            }

            /* Read the MQCIH header */
            byte[] mqcihHost = new byte[MQCIH_STRUCLENGTH];
            jmsMessage.readBytes(mqcihHost, MQCIH_STRUCLENGTH);
            Mqcih mqcihJava = getMqcihTransformers().toJava(mqcihHost,
                    hostCharset);

            /*
             * Check that execution worked. If not, the message content is an
             * explicit error message
             */
            if (mqcihJava.getMqcihCompcode() != 0) {
                byte[] errorBytes = new byte[dataLength - MQCIH_STRUCLENGTH];
                jmsMessage.readBytes(errorBytes);
                throw new HostReceiveException(new String(errorBytes,
                        hostCharset));
            }

            /*
             * When binary data is returned following MQCIH format should be
             * none
             */
            if (!mqcihJava.getMqcihFormat().equals(MQFMT_NONE.trim())) {
                throw new HostReceiveException("Reply data has format "
                        + mqcihJava.getMqcihFormat()
                        + " when none was expected");
            }

            /* Read the program ID */
            byte[] programNameHost = new byte[CICS_PROGRAM_NAME_LEN];
            jmsMessage.readBytes(programNameHost, CICS_PROGRAM_NAME_LEN);

            /* Finally get the reply commarea raw data */
            byte[] replyCommarea = new byte[dataLength - MQCIH_STRUCLENGTH
                    - CICS_PROGRAM_NAME_LEN];
            jmsMessage.readBytes(replyCommarea);

            /* Stuff it into a LegStar message */
            LegStarMessage reponseMessage = new LegStarMessage();
            reponseMessage.addDataPart(new CommareaPart(replyCommarea));

            return reponseMessage;

        } catch (HeaderPartException e) {
            throw new HostReceiveException(e);
        } catch (IOException e) {
            throw new HostReceiveException(e);
        } catch (HostTransformException e) {
            throw new HostReceiveException(e);
        } catch (JMSException e) {
            throw new HostReceiveException(e);
        }

    }

    /**
     * @return COBOL Binding transformer for MQCIH
     */
    public MqcihTransformers getMqcihTransformers() {
        return _MqcihTransformers;
    }

    /**
     * Create an MQ CICS Bridge header.
     * 
     * @param outputdatalength the output commarea length plus 8 characters for
     *            the program name (must be lower or equals to 32767).
     * @param password used for authentication
     * @return an MQCIH header
     */
    private Mqcih getMqcih(final int outputdatalength, final String password) {
        Mqcih mqcih = new Mqcih();
        mqcih.setMqcihOutputdatalength(outputdatalength);
        mqcih.setMqcihAuthenticator(password);
        mqcih.setMqcihGetwaitinterval((int) getReceiveTimeout());
        mqcih.setMqcihUowcontrol(MQCUOWC_ONLY);
        mqcih.setMqcihFlags(MQCIH_REPLY_WITHOUT_NULLS + MQCIH_SYNC_ON_RETURN);
        mqcih.setMqcihReplytoformat(MQFMT_NONE);
        return mqcih;
    }

    /**
     * The mainframe CCSID can be derived from the java character set. TODO Find
     * a more reliable way of deriving CCSID from java charset name.
     * 
     * @param hostCharset the mainframe character set
     * @return the MQ CCSID
     */
    private int getCCSID(final String hostCharset) {
        if (hostCharset != null) {
            if (hostCharset.equals("IBM-Thai")) {
                return 838;
            }
            if (hostCharset.startsWith("IBM")) {
                return Integer.parseInt(hostCharset.substring(3));
            }
            if (hostCharset.startsWith("x-IBM")) {
                return Integer.parseInt(hostCharset.substring(5));
            }
        }
        return 500;
    }

    /**
     * The java character set is derived from the CCSID. TODO Find a more
     * reliable way of deriving java charset name from CCSID.
     * 
     * @param ccsid the mainframe CCSID
     * @return the java charset name
     */
    private String getHostCharset(final int ccsid) {
        String num = Integer.toString(ccsid);
        if (num.startsWith("11") || num.equals("37")) {
            return "IBM0" + num;
        }
        if (num.equals("838")) {
            return "IBM-Thai";
        }
        return "IBM" + Integer.toString(ccsid);
    }
}
