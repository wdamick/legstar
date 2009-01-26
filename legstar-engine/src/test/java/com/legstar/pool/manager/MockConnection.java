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
package com.legstar.pool.manager;

import java.util.UUID;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;

/**
 * Simulates a transport.
 *
 */
public class MockConnection implements LegStarConnection {
    
    /** A unique ID for this connection.*/
    private String mConnectionID;

    /** No-arg constructor. */
    public MockConnection() {
        mConnectionID = UUID.randomUUID().toString();
    }
    
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
        return mConnectionID;
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
            if (request.getID().equals("testScheduleFailingWork")) {
                throw new RequestException(
                        "CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=12");
            }
            LegStarMessage responseMessage = new LegStarMessage();
            responseMessage.addDataPart(new CommareaPart(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexReply100())));
            request.setResponseMessage(responseMessage);
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
