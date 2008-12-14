/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.messaging;

import java.io.InputStream;
import java.io.SequenceInputStream;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;

/**
 * Messages represents the input and output of requests. A message is composed
 * of message parts: one header part and any number of data message parts.
 */
public class LegStarMessage implements Serializable {

    /** Serial version ID.  */
    private static final long serialVersionUID = -1107635334404959251L;

    /** Header message part. */
    private LegStarHeaderPart mHeaderPart;

    /** Data message parts. */
    private List < LegStarMessagePart > mDataParts;

    /**
     * Creates an empty message.
     * @throws HeaderPartException if host encoding is wrong
     */
    public LegStarMessage() throws HeaderPartException {
        mHeaderPart = new LegStarHeaderPart();
        mDataParts = new ArrayList < LegStarMessagePart >();
    }

    /**
     * Construct a message from its message parts.
     * @param headerPart the header message part
     * @param dataParts the data message parts
     */
    public LegStarMessage(
            final LegStarHeaderPart headerPart,
            final List < LegStarMessagePart > dataParts) {
        mHeaderPart = headerPart;
        mDataParts = dataParts;
    }

    /**
     * Streaming an entire message is equivalent to streaming its header part
     * followed by each of the data parts.
     * @return an input stream
     * @throws UnsupportedEncodingException if conversion fails
     */
    public final InputStream sendToHost() throws UnsupportedEncodingException {
        Vector < InputStream > v = new Vector < InputStream >();
        v.add(mHeaderPart.sendToHost());
        for (LegStarMessagePart part : mDataParts) {
            v.add(part.sendToHost());
        }
        Enumeration < InputStream > e = v.elements();
        return new SequenceInputStream(e);
    }

    /**
     * Recreates the message by creating each part.
     * @param hostStream the host byte stream
     * @throws HostReceiveException if creation fails
     */
    public final void recvFromHost(
            final InputStream hostStream) throws HostReceiveException {
        mDataParts = new ArrayList < LegStarMessagePart >(); 
        mHeaderPart.recvFromHost(hostStream);
        for (int i = 0; i < mHeaderPart.getDataPartsNumber(); i++) {
            LegStarMessagePart part = new LegStarMessagePart();
            part.recvFromHost(hostStream);
            mDataParts.add(part);
        }
    }

    /**
     * @return the size in bytes of this message host serialization
     */
    public final int getHostSize() {
        int size = mHeaderPart.getHostSize();
        for (LegStarMessagePart part : mDataParts) {
            size += part.getHostSize();
        }
        return size;
    }

    /**
     * @return the list of data message parts
     */
    public final List < LegStarMessagePart > getDataParts() {
        return mDataParts;
    }

    /**
     * Look for a part identified with a specific ID (usually a container 
     * name).
     * @param partID part identifier
     * @return the part found or null otherwise
     */
    public final LegStarMessagePart lookupDataPart(final String partID) {
        for (LegStarMessagePart part : getDataParts()) {
            if (part.getID().equals(partID)) {
                return part;
            }
        }
        return null;
    }

    /**
     * Add a new data part. This assumes a header part has already been created.
     * @param part the data part to add.
     */
    public final void addDataPart(final LegStarMessagePart part) {
        mDataParts.add(part);
        mHeaderPart.setDataPartsNumber(mHeaderPart.getDataPartsNumber() + 1);
    }

    /**
     * @param dataParts the list of data message parts to set
     */
    public final void setDataParts(
            final List < LegStarMessagePart > dataParts) {
        mDataParts = dataParts;
    }

    /**
     * @return the header message part
     */
    public final LegStarHeaderPart getHeaderPart() {
        return mHeaderPart;
    }

    /**
     * @param headerPart the header message part to set
     */
    public final void setHeaderPart(final LegStarHeaderPart headerPart) {
        mHeaderPart = headerPart;
    }

    /** {@inheritDoc} */
    public final String toString() {
        StringBuffer sb = new StringBuffer(80);
        sb.append(this.getClass().getSimpleName());
        sb.append("{this=").append(Integer.toHexString(
                System.identityHashCode(this)));
        sb.append(", headerPart=").append(mHeaderPart.toString());
        for (int i = 0; i < mHeaderPart.getDataPartsNumber(); i++) {
            LegStarMessagePart part = mDataParts.get(i);
            sb.append(", messagePart=").append(part.toString());
        }
        sb.append("\"}");
        return sb.toString();                        
    }

    /**
     * Two messages are equal if headers and all parts are equal.
     * @param obj message part to compare to
     * @return true if message part compared to has same id and content.
     */
    public final boolean equals(final Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof LegStarMessage)) {
            return false;
        }
        LegStarMessage msg = (LegStarMessage) obj;
        if (!msg.getHeaderPart().equals(getHeaderPart())) {
            return false;
        }
        if (msg.getDataParts().size() != getDataParts().size()) {
            return false;
        }
        for (int i = 0; i < msg.getDataParts().size(); i++) {
            if (!msg.getDataParts().get(i).equals(
                    getDataParts().get(i))) {
                return false;
            }
        }
        return true;
    }

    /**
     * @see Object#hashCode() 
     * {@inheritDoc}
     */
    public final int hashCode() {
        int hash = getHeaderPart().hashCode();
        for (int i = 0; i < getDataParts().size(); i++) {
            hash += getDataParts().get(i).hashCode();
        }
        return hash;
    }

}
