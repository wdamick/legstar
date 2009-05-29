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
package com.legstar.messaging;


/**
 * Abstract description of a host connection.
 */
public interface LegStarConnection {

    /**
     * Get this connection unique ID.
     * @return the unique ID
     *  */
    String getConnectionID();

    /**
     * Connect to the host with the given credentials.
     * @param password host password (null to use default)
     * @throws ConnectionException if connection fails
     */
    void connect(String password) throws ConnectionException;

    /**
     * Connect to host trying to reuse connection if possible.
     * @param password host password (null to use default)
     * @throws ConnectionException if connection fails
     */
    void connectReuse(String password) throws ConnectionException;

    /**
     * Send a request to the host. This is non-blocking.
     * @param request the request to be serviced
     * @throws RequestException if send fails
     */
    void sendRequest(LegStarRequest request) throws RequestException;

    /**
     * Receive a message part. This method blocks until a message is available.
     * @param request the request being serviced
     * @throws RequestException if response cannot be received
     */
    void recvResponse(LegStarRequest request) throws RequestException;

    /**
     * Request a commit from the host.
     * @throws RequestException if commit fails
     */
    void commitUOW() throws RequestException;

    /**
     * Request continuation of the same unit of work from the host.
     * @throws RequestException if keep fails
     */
    void keepUOW() throws RequestException;

    /**
     * Request a unit of work rollback from the host.
     * @throws RequestException if rollback fails
     */
    void rollbackUOW() throws RequestException;

    /**
     * Close the host connection.
     * @throws RequestException if close fails
     */
    void close() throws RequestException;

    /**
     * While trying to establish a connection with the host, this sets
     * a limit to the time we are prepared to wait for a successful
     * connection.
     * @param timeout maximum time (milliseconds) to wait for connection
     */
    void setConnectTimeout(long timeout);

    /**
     * Connection timeout is the maximum time to wait for a connection
     * to succeed.
     * @return the current connection timeout value
     */
    long getConnectTimeout();

    /**
     * While waiting for the host to reply to a request, this sets
     * a limit to the time we are prepared to wait for data back.
     * @param timeout maximum time (milliseconds) to wait for host reply
     */
    void setReceiveTimeout(long timeout);

    /**
     * Receive timeout is the maximum time to wait for a reply from the
     * host.
     * @return the current receive timeout
     */
    long getReceiveTimeout();
}
