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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONException;
import org.json.JSONObject;

import com.legstar.config.Constants;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

/**
 * Simulates a transport.
 *
 */
public class MockConnection implements LegStarConnection {

    /** Logger. */
    private final Log _log = LogFactory.getLog(MockConnection.class);

    /** Connection ID. */
    private String _connectionID;
    
    /**
     * No-arg constructor.
     */
    public MockConnection() {
        
    }

    /**
     * Create with and ID.
     * @param connectionID the connection ID
     */
    public MockConnection(final String connectionID) {
        _connectionID = connectionID;
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
        return _connectionID;
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
            String jsonString = request.getRequestMessage().getHeaderPart().getJsonString();

            if (_log.isDebugEnabled()) {
                _log.debug("Mocking program execution: " + jsonString);
            }
            /* Get the request header json string. we need the target program ID*/
            JSONObject jsonObj = new JSONObject(jsonString);
            String programName = (String) jsonObj.get(Constants.CICS_PROGRAM_NAME_KEY);
            if (programName.equals("LSFILEAE")) {
                request.setResponseMessage(MockLsfileae.getResponse(request.getRequestMessage()));
            } else if (programName.equals("LSFILEAC")) {
                request.setResponseMessage(MockLsfileac.getResponse(request.getRequestMessage()));
            } else if (programName.equals("LSFILEAL")) {
                request.setResponseMessage(MockLsfileal.getResponse(request.getRequestMessage()));
            } else if (programName.equals("VARARCOM")) {
                request.setResponseMessage(MockVararcom.getResponse(request.getRequestMessage()));
            } else if (programName.equals("T1SLEEPT")) {
                request.setResponseMessage(MockT1sleep.getResponse(request.getRequestMessage()));
            } else {
                throw new RequestException("CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=3");
            }
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        } catch (JSONException e) {
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
