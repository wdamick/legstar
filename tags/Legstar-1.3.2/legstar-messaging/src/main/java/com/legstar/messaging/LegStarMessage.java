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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

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
     * @throws HostMessageFormatException if conversion fails
     */
    public InputStream sendToHost() throws HostMessageFormatException {
        return new ByteArrayInputStream(toByteArray());
    }

    /**
     * Recreates the message by creating each part.
     * @param hostStream the host byte stream
     * @throws HostMessageFormatException if creation fails
     */
    public void recvFromHost(
        final InputStream hostStream) throws HostMessageFormatException {
        getHeaderPart().fromStream(hostStream);
        for (int i = 0; i < getHeaderPart().getDataPartsNumber(); i++) {
            LegStarMessagePart part = new LegStarMessagePart();
            part.fromStream(hostStream);
            getDataParts().add(part);
        }
    }

    /**
     * @return the size in bytes of this message host serialization
     */
    public int getHostSize() {
        int size = mHeaderPart.getHostSize();
        for (LegStarMessagePart part : mDataParts) {
            size += part.getHostSize();
        }
        return size;
    }
    
    /**
     * This form returns the message content serialized in a new byte array.
     * @return a byte array with complete message serialized ready for transmission
     * @throws HostMessageFormatException if message have format issues
     */
    public byte[] toByteArray() throws HostMessageFormatException {
        byte[] payload = new byte[getHostSize()];
        int pos = 0;
        pos = getHeaderPart().toByteArray(payload, pos);
        for (LegStarMessagePart part : getDataParts()) {
            pos = part.toByteArray(payload, pos);
        }
        return payload;
    }

    /**
     * Deserialize this message from a byte array originating from a host.
     * @param src the byte array of host data
     * @param srcPos where to start deserializing in the byte array
     * @return the new position in the byte array after this message was deserialized
     * @throws HostMessageFormatException if deserialization fails
     */
    public int fromByteArray(
            final byte[] src, final int srcPos) throws HostMessageFormatException {
        int pos = 0;
        pos = getHeaderPart().fromByteArray(src, pos);
        for (int i = 0; i < getHeaderPart().getDataPartsNumber(); i++) {
            LegStarMessagePart part = new LegStarMessagePart();
            pos = part.fromByteArray(src, pos);
            getDataParts().add(part);
        }
        return pos;
    }

    /**
     * @return the list of data message parts
     */
    public List < LegStarMessagePart > getDataParts() {
        return mDataParts;
    }

    /**
     * Look for a part identified with a specific ID (usually a container 
     * name).
     * @param partID part identifier
     * @return the part found or null otherwise
     */
    public LegStarMessagePart lookupDataPart(final String partID) {
        for (LegStarMessagePart part : getDataParts()) {
            if (part.getPartID().equals(partID)) {
                return part;
            }
        }
        return null;
    }

    /**
     * Add a new data part. This assumes a header part has already been created.
     * @param part the data part to add.
     */
    public void addDataPart(final LegStarMessagePart part) {
        mDataParts.add(part);
        mHeaderPart.setDataPartsNumber(mHeaderPart.getDataPartsNumber() + 1);
    }

    /**
     * @param dataParts the list of data message parts to set
     */
    public void setDataParts(
            final List < LegStarMessagePart > dataParts) {
        mDataParts = dataParts;
    }

    /**
     * @return the header message part
     */
    public LegStarHeaderPart getHeaderPart() {
        return mHeaderPart;
    }

    /**
     * @param headerPart the header message part to set
     */
    public void setHeaderPart(final LegStarHeaderPart headerPart) {
        mHeaderPart = headerPart;
    }

    /** {@inheritDoc} */
    public String toString() {
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
    public boolean equals(final Object obj) {
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
    public int hashCode() {
        int hash = getHeaderPart().hashCode();
        for (int i = 0; i < getDataParts().size(); i++) {
            hash += getDataParts().get(i).hashCode();
        }
        return hash;
    }
    
    /**
     * Checks if a payload originating from a mainframe is a formatted
     * LegStarMessage.
     * @param payload the payload to check
     * @return true if the payload is a LegStarMessage
     * @throws UnsupportedEncodingException if unable to read payload data
     */
    public static boolean isLegStarMessage(
            final byte[] payload) throws UnsupportedEncodingException {
        return LegStarHeaderPart.isLegStarHeader(payload);
    }

    /**
     * Retrieve the content from the first part of a serialized LegStar message.
     * Only single part messages are supported.
     * @param requestBytes the host request data
     * @return the request content
     * @throws HostMessageFormatException if format error
     */
    public static byte[] getContentFromHostBytes(
            final byte[] requestBytes) throws HostMessageFormatException {
        try {
            LegStarMessage legStarMessage = new LegStarMessage();
            legStarMessage.fromByteArray(requestBytes, 0);
            if (legStarMessage.getDataParts().size() == 0) {
                return new byte[0];
            } else if (legStarMessage.getDataParts().size() == 1) {
                return legStarMessage.getDataParts().get(0).getContent();
            } else {
                throw new HostMessageFormatException("Multi-part messages not supported");
            }
        } catch (HeaderPartException e) {
            throw new HostMessageFormatException(e);
        }
    }
    
    /**
     * Create a single part LegStarMessage from a byte array with default
     * header.
     * @param content the serialized part content ready for transport
     * @return a serialized LegStarMessage ready for transport
     * @throws HostMessageFormatException if message formatting fails
     */
    public static byte[] getHostBytesFromContent(
            final byte[] content) throws HostMessageFormatException {
        try {
            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new CommareaPart(content));
            return replyMessage.toByteArray();
        } catch (HeaderPartException e) {
            throw new HostMessageFormatException(e);
        }
        
    }

}
