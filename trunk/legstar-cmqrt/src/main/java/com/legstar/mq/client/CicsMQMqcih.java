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

import org.json.JSONException;
import org.json.JSONObject;

import com.ibm.mq.MQC;
import com.ibm.mq.MQMessage;
import com.legstar.codec.HostCodec;
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
 * Implementation of MQ connection used when the mainframe runs IBM CICS MQ Bridge.
 * <p/>
 * The MQ messages payloads exchanged with the mainframe starts with an MQCIH header
 * followed by the program name and finally the raw data.
 *
 */
public class CicsMQMqcih extends AbstractCicsMQ  {

    /** COBOL Binding transformer for MQCIH. */
    private MqcihTransformers _MqcihTransformers;

    /** While we find this constant somewhere in MQ.*/
    private static final int MQCIH_STRUCLENGTH = 180;
    
    /** Flag indicating that reply should not include trailing low-values. */
    private static final int MQCIH_REPLY_WITHOUT_NULLS = 2;

    /** Flag indicating that a syncpoint should be taken upon return from 
     * execution of DPL program.
     * TODO revise when we implement single phase commit */
    private static final int MQCIH_SYNC_ON_RETURN = 4;

    /** Means that the DPL interaction is the only one with the UOW..
     * TODO revise when we implement single phase commit */
    private static final int MQCUOWC_ONLY = 273;

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
    public CicsMQMqcih(
            final String connectionID,
            final CicsMQEndpoint cicsMQEndpoint,
            final int connectionTimeout,
            final int receiveTimeout) throws CicsMQConnectionException {
        super(connectionID, cicsMQEndpoint, connectionTimeout, receiveTimeout);
        _MqcihTransformers = new MqcihTransformers();
    }

    /**
     * Creates an MQ request message with appropriate header data.
     * A request is folded as MQ Headers and a binary payload.
     * <p/>
     * The IBM CICS MQ Bridge cannot support channel/containers. 
     * @param request request description
     * @return the MQ message
     * @throws RequestException if formatting of mq message fails
     */
    public MQMessage createMQRequestMessage(
            final LegStarRequest request) throws RequestException {

        if (request.getRequestMessage().getDataParts().size() > 1) {
            throw new RequestException(
                    "IBM CICS MQ Bridge does not support channel/containers");
        }
        
        String hostCharset = request.getAddress().getHostCharset();
        String hostUserID = request.getAddress().getHostUserID();
        String hostPassword = request.getAddress().getHostPassword();

        try {
            MQMessage mqMessage = new MQMessage();

            /* There is no support for multiple message UOW. We assume this
             * is the start of new UOW.  */
            mqMessage.messageId = MQC.MQCI_NONE;
            mqMessage.correlationId = MQC.MQCI_NEW_SESSION;
 
            /* Specify usage of CICS MQ Bridge. */
            mqMessage.format = MQC.MQFMT_CICS;
           
            /* The IBM CICS Bridge will attempt to convert the message content
             * so we need to instruct him that this is already in the target
             * host character set.*/
            mqMessage.characterSet = getCCSID(hostCharset);

            /* set credentials. */
            mqMessage.userId = hostUserID;

            /* Get parameters from the LegStar header */
            JSONObject jsonObj = new JSONObject(
                    request.getRequestMessage().getHeaderPart().getJsonString());
            String programName = (String) jsonObj.get(Constants.CICS_PROGRAM_NAME_KEY);

            /* The output data length must account for the 8 characters program name */
            int outputdatalength = jsonObj.getInt(Constants.CICS_LENGTH_KEY) + 8;
            
            /* Finally create the mq message content: MQCIH + PROGRAM + COMMAREA */
            mqMessage.write(getMqcihTransformers().toHost(
                    getMqcih(outputdatalength, hostPassword), hostCharset));
            mqMessage.write(programName.getBytes(HostCodec.HEADER_CODE_PAGE));
            mqMessage.write(request.getRequestMessage().getDataParts().get(0).getContent());

            return mqMessage;
            
        } catch (IOException e) {
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
     * Creates a response message from the MQ reply back.
     * The MQ payload should start with an MQCIH header followed by the
     * program name and the raw data.
     * @param mqMessage the MQ response message
     * @return a response message
     * @throws HostReceiveException if response cannot be mapped to a message
     */
    public LegStarMessage createResponseMessage(
            final MQMessage mqMessage) throws HostReceiveException {

        /* Check that reply has an MQCIH header */
        if (!mqMessage.format.equals(MQC.MQFMT_CICS)) {
            throw new HostReceiveException("Reply does not hold an MQCIH header");
        }
        if (mqMessage.getTotalMessageLength() < MQCIH_STRUCLENGTH) {
            throw new HostReceiveException("Reply too small to hold a MQCIH header");
        }
        try {
            /* Read the MQCIH header */
            byte[] mqcihHost = new byte[MQCIH_STRUCLENGTH];
            mqMessage.readFully(mqcihHost, 0, MQCIH_STRUCLENGTH);
            Mqcih mqcihJava = getMqcihTransformers().toJava(mqcihHost,
                    getHostCharset(mqMessage.characterSet));

            /* Check that execution worked. If not, the message content is an
             * explicit error message */
            if (mqcihJava.getMqcihCompcode() != 0) {
                throw new HostReceiveException(
                        mqMessage.readStringOfByteLength(mqMessage.getDataLength()));
            }
            /* When binary data is returned following MQCIH format should be none */
            if (!mqcihJava.getMqcihFormat().equals(MQC.MQFMT_NONE.trim())) {
                throw new HostReceiveException("Reply data has format " + mqcihJava.getMqcihFormat()
                        + " when none was expected");
            }

            /* Read the program ID */
            byte[] programNameHost = new byte[8];
            mqMessage.readFully(programNameHost, 0, 8);

            /* Finally get the reply commarea raw data */
            byte[] replyCommarea = new byte[mqMessage.getDataLength()];
            mqMessage.readFully(replyCommarea);

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
     * @param outputdatalength the output commarea length plus 8 characters
     *  for the program name (must be lower or equals to 32767).
     * @param password used for authentication
     * @return an MQCIH header
     */
    private Mqcih getMqcih(
            final int outputdatalength,
            final String password) {
        Mqcih mqcih = new Mqcih();
        mqcih.setMqcihOutputdatalength(outputdatalength);
        mqcih.setMqcihAuthenticator(password);
        mqcih.setMqcihGetwaitinterval((int) getReceiveTimeout());
        mqcih.setMqcihUowcontrol(MQCUOWC_ONLY);
        mqcih.setMqcihFlags(MQCIH_REPLY_WITHOUT_NULLS + MQCIH_SYNC_ON_RETURN);
        mqcih.setMqcihReplytoformat(MQC.MQFMT_NONE);
        return mqcih;
    }

    /**
     * The mainframe CCSID can be derived from the java character set.
     * TODO Find a more reliable way of deriving CCSID from java charset name.
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
     * The java character set is derived from the CCSID.
     * TODO Find a more reliable way of deriving java charset name from CCSID.
     * @param ccsid the mainframe CCSID
     * @return the java charset name
     */
    private String getHostCharset(final int ccsid) {
        String num = Integer.toString(ccsid);
        if (num.startsWith("11") || num.equals("37")) {
            return "IBM0" + num;
        }
        if (num.equals(838)) {
            return "IBM-Thai";
        }
        return "IBM" + Integer.toString(ccsid);
    }
}
