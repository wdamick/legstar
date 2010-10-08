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
package com.legstar.messaging;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * This class represents a request for its entire lifetime. It is created
 * by a client with input data and then processed by the server which
 * attaches reply data or an exception as appropriate.
 */
public class LegStarRequest {

    /** A unique ID for this request. */
    private String mID;

    /** The request message. */
    private LegStarMessage mRequestMessage;

    /** The response message. */
    private LegStarMessage mResponseMessage;

    /** If request processing failed, this will describe the failure. */
    private Exception mException;

    /** Host destination for this request. */
    private LegStarAddress mAddress;

    /** This allows users to attach unstructured data to a particular request.*/
    private byte[] mAttachment;

    /** This is used to signal that this request is being processed. Multiple 
     * threads might be waiting for completion. */
    private CountDownLatch mProcessingLatch = null;

    /** Creates an empty request. */
    public LegStarRequest() {
    }

    /**
     * Constructor for a new request.
     * @param id a unique ID for this request
     * @param address the host destination
     * @param requestMessage the input message
     */
    public LegStarRequest(
            final String id,
            final LegStarAddress address,
            final LegStarMessage requestMessage) {
        mID = id;
        mAddress = address;
        mRequestMessage = requestMessage;
    }

    /**
     * @return the unique ID for this request
     */
    public String getID() {
        return mID;
    }

    /**
     * @param id the unique ID for this request to set
     */
    public void setID(final String id) {
        mID = id;
    }

    /**
     * @return the the input message
     */
    public LegStarMessage getRequestMessage() {
        return mRequestMessage;
    }

    /**
     * @param requestMessage the input message to set
     */
    public void setRequestMessage(final LegStarMessage requestMessage) {
        mRequestMessage = requestMessage;
    }

    /**
     * @return the reply message
     */
    public LegStarMessage getResponseMessage() {
        return mResponseMessage;
    }

    /**
     * @param responseMessage the reply message to set
     */
    public void setResponseMessage(final LegStarMessage responseMessage) {
        mResponseMessage = responseMessage;
    }

    /**
     * @return the work exception
     */
    public Exception getException() {
        return mException;
    }

    /**
     * @param requestException the work exception to set
     */
    public void setException(final Exception requestException) {
        mException = requestException;
    }

    /**
     * @return the host destination
     */
    public LegStarAddress getAddress() {
        return mAddress;
    }

    /**
     * @param address the host destination to set
     */
    public void setAddress(final LegStarAddress address) {
        mAddress = address;
    }

    /**
     * @return the data attached to the request
     */
    public byte[] getAttachment() {
        return mAttachment;
    }

    /**
     * @param attachment the data to attach to the request
     */
    public void setAttachment(final byte[] attachment) {
        mAttachment = attachment;
    }

    /**
     * Used to signal that this request is now being processed.
     */
    public void signalProcessingStart() {
        mProcessingLatch = new CountDownLatch(1);
    }

    /**
     * Used to signal that this request has been processed. It might have
     * failed though so checking for an exception is a good idea.
     */
    public void signalProcessingStop() {
        mProcessingLatch.countDown();
    }

    /**
     * Allows threads to wait for this request completion.
     * @param timeout how long to wait
     * @param unit the timeout unit
     * @return true if request was processed and false if the waiting time
     *  elapsed before the request was processed.
     * @throws InterruptedException if thread was interrupted
     */
    public boolean await(
            final long timeout,
            final TimeUnit unit) throws InterruptedException {
        if (mProcessingLatch == null) {
            return true;
        } else {
            return mProcessingLatch.await(timeout, unit);
        }
    }

}
