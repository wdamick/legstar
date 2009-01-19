package com.legstar.host;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileacCases;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.VararcomCases;

/**
 * Simulates a transport.
 *
 */
public class MockConnection implements LegStarConnection {

    /** {@inheritDoc} */
    public void close() throws RequestException {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    public void commitUOW() throws RequestException {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    public void connect(final String password) throws ConnectionException {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    public void connectReuse(final String password) throws ConnectionException {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    public long getConnectTimeout() {
        // TODO Auto-generated method stub
        return 0;
    }

    /** {@inheritDoc} */
    public String getConnectionID() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public long getReceiveTimeout() {
        // TODO Auto-generated method stub
        return 0;
    }

    /** {@inheritDoc} */
    public void keepUOW() throws RequestException {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    public void recvResponse(final LegStarRequest request) throws RequestException {
        try {
            if (request.getID().equals("Lsfileae100")) {
                LegStarMessage responseMessage = new LegStarMessage();
                responseMessage.addDataPart(new CommareaPart(
                        HostData.toByteArray(LsfileaeCases.getHostBytesHexReply100())));
                request.setResponseMessage(responseMessage);
            } else if (request.getID().equals("WrongProgram")) {
                throw new RequestException("CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=12");
            } else if (request.getID().equals("Vararcom")) {
                LegStarMessage responseMessage = new LegStarMessage();
                responseMessage.addDataPart(new CommareaPart(
                        HostData.toByteArray(VararcomCases.getHostBytesHex36())));
                request.setResponseMessage(responseMessage);
            } else if (request.getID().equals("Lsfileac")) {
                LegStarMessage responseMessage = new LegStarMessage();
                responseMessage.addDataPart(new ContainerPart("ReplyData",
                        HostData.toByteArray(LsfileacCases.getHostBytesHexReplyData())));
                responseMessage.addDataPart(new ContainerPart("ReplyStatus",
                        HostData.toByteArray(LsfileacCases.getHostBytesHexReplyStatus())));
                request.setResponseMessage(responseMessage);
            } else if (request.getID().equals("LsfileacEmpty")) {
                LegStarMessage responseMessage = new LegStarMessage();
                responseMessage.addDataPart(new ContainerPart("ReplyData",
                        null));
                responseMessage.addDataPart(new ContainerPart("ReplyStatus",
                        HostData.toByteArray(LsfileacCases.getHostBytesHexReplyStatusNoMatch())));
                request.setResponseMessage(responseMessage);
            }
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        }
        
    }

    /** {@inheritDoc} */
    public void rollbackUOW() throws RequestException {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    public void sendRequest(final LegStarRequest request) throws RequestException {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    public void setConnectTimeout(final long timeout) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    public void setReceiveTimeout(final long timeout) {
        // TODO Auto-generated method stub
        
    }

}
