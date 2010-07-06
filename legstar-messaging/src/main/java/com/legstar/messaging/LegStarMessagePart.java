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

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.Arrays;

import com.legstar.codec.HostCodec;
import com.legstar.coxb.host.HostData;

/**
 * MessageParts are generic named, binary containers used to send
 * and receive from host.
 */
public class LegStarMessagePart implements Serializable {

    /** Serial version ID.  */
    private static final long serialVersionUID = -2361247952255347371L;

    /** The message part identifier. */
    private String mPartID;

    /** The message content. */
    private byte[] mContent;

    /** This is the amount of actual data in mContent. This allows
     * mContent to be larger than the actual payload. Payload can
     * dynamically vary for variable size structures. */
    private int mPayloadSize;

    /** This indicates if the payload size was explicitly set. If not,
     * then the payload size will be identical to mContent.length. */
    private boolean mPayloadSizeSet = false;

    /** Size of message part identifier on host (bytes). */
    private static final int MSG_PART_ID_LEN = 16;

    /** Size of content length on host (bytes). */
    private static final int CONTENT_LEN_LEN = 4;

    /** The error identifier that the host will send back.*/
    private static final String ERROR_ID = "LSOKERR0";

    /** This upper limit to part length is necessary to protect against totally
     * corrupt data coming from the host that might otherwise result in huge
     * memory allocations.*/
    private static final int MAX_CONTENT_LEN = 16777216;
    
    /**
     * Create an empty message part.
     */
    public LegStarMessagePart() {
        mPartID = null;
    }

    /**
     * Create a named message part from a content.
     * @param id the message part identifier
     * @param content a binary content
     */
    public LegStarMessagePart(final String id, final byte[] content) {
        mPartID = id;
        mContent = content;
    }

    /**
     * @return the message content
     */
    public byte[] getContent() {
        return mContent;
    }

    /**
     * @param content the message content to set
     */
    public void setContent(final byte[] content) {
        mContent = content;
    }

    /**
     * @return the message part identifier
     */
    public String getPartID() {
        return mPartID;
    }

    /**
     * If this is a typed message part (such as a header part),
     * check that the ID received match the expected one. Otherwise
     * set the current part ID with the one received.
     * @param partID the part ID received
     * @throws HostMessageFormatException if the part ID received does not match
     *  the expected part ID
     */
    public void setPartID(
            final String partID) throws HostMessageFormatException {

        if (getPartID() == null) {
            if (partID.length() > MSG_PART_ID_LEN) {
                throw new HostMessageFormatException(
                        "Invalid message part ID " + partID);
            }
            mPartID = partID;
        } else {
            if (!getPartID().equals(partID)) {
                throw new HostMessageFormatException(
                        "Invalid message part ID. Expected " + getPartID()
                        + ", received " + partID);
            }
        }
    }
    
    /**
     * @return the size in bytes of this message part host serialization
     */
    public int getHostSize() {
        return (MSG_PART_ID_LEN + CONTENT_LEN_LEN + getPayloadSize());
    }

    /**
     * This form returns the message part content serialized in a new byte array.
     * @return a byte array with complete message part serialized ready for transmission
     * @throws HostMessageFormatException if message have format issues
     */
    public byte[] toByteArray() throws HostMessageFormatException {
        byte[] payload = new byte[getHostSize()];
        toByteArray(payload, 0);
        return payload;
    }

    /**
     * This forms contributes this message part to a byte array.
     * @param dest the destination byte array being populated
     * @param destPos the position to start from in destination byte array
     * @return the new position in the byte array
     * @throws HostMessageFormatException if message have format issues
     */
    public int toByteArray(
            final byte[] dest, final int destPos) throws HostMessageFormatException {
        /* Make sure there is room for us */
        if ((dest.length - destPos) < getHostSize()) {
            throw (new HostMessageFormatException("Destination byte array too small"));
        }
        try {
            int pos = destPos;
            HostCodec.toHostBytes(mPartID, dest, pos,
                    MSG_PART_ID_LEN, HostCodec.HEADER_CODE_PAGE);
            pos += MSG_PART_ID_LEN;
            ByteBuffer bb = ByteBuffer.allocate(4);
            bb.putInt(getPayloadSize());
            bb.flip();
            bb.get(dest, pos, CONTENT_LEN_LEN);
            pos += CONTENT_LEN_LEN;
            if (getContent() != null) {
                System.arraycopy(getContent(), 0, dest, pos, getPayloadSize());
            }
            return pos + getPayloadSize();
            
        } catch (UnsupportedEncodingException e) {
            throw new HostMessageFormatException(e);
        }
    }
    
    /**
     * Deserialize this message part from a byte array originating from a host.
     * @param src the byte array of host data
     * @param srcPos where to start deserializing in the byte array
     * @return the new position in the byte array after this message part was deserialized
     * @throws HostMessageFormatException if deserialization fails
     */
    public int fromByteArray(
            final byte[] src, final int srcPos) throws HostMessageFormatException {
        try {
            /* At a minimum, there should be enough bytes for an error ID */
            if ((src.length - srcPos) < ERROR_ID.length()) {
                throw new HostMessageFormatException("Invalid message part");
            }
            /* Is this an error report rather than a message part? */
            String errorId = new String(src, srcPos, ERROR_ID.length(),
                    HostCodec.HEADER_CODE_PAGE).trim();
            if (errorId.equals(ERROR_ID)) {
                throw getHostException(src, srcPos);
            }
            /* Now we must have at least a part ID followed by a part length */
            if ((src.length - srcPos) < (MSG_PART_ID_LEN + CONTENT_LEN_LEN)) {
                throw new HostMessageFormatException("Invalid message part. No ID");
            }
            /* It is now safe to get a part constituents */
            int pos = srcPos;
            String partId = new String(src, pos, MSG_PART_ID_LEN,
                    HostCodec.HEADER_CODE_PAGE).trim();
            setPartID(partId);
            pos += MSG_PART_ID_LEN;
            int contentLen = getContentLength(src, pos);
            pos += CONTENT_LEN_LEN;
            return setContent(src, pos, contentLen);
            
        } catch (UnsupportedEncodingException e) {
            throw new HostMessageFormatException(e);
        }
    }
    
    /**
     * Deserialize this message part from a stream originating from a host.
     * @param stream the stream of host data
     * @throws HostMessageFormatException if deserialization fails
     */
    public void fromStream(
            final InputStream stream) throws HostMessageFormatException {
        try {
            byte[] partIDBytes = new byte[MSG_PART_ID_LEN];
            /* Start by looking for an error marker (note that it is shorter than part ID) */
            int readBytes = stream.read(partIDBytes, 0, ERROR_ID.length());
            if (readBytes < ERROR_ID.length()) {
                throw new HostMessageFormatException("Invalid message part");
            }
            /* Is this an error report rather than a message part? */
            String errorId = new String(partIDBytes, 0, ERROR_ID.length(),
                    HostCodec.HEADER_CODE_PAGE).trim();
            if (errorId.equals(ERROR_ID)) {
                throw getHostException(stream);
            }
            /* Now we must have at least a part ID so read it completely */
            readBytes += stream.read(partIDBytes, ERROR_ID.length(),
                    MSG_PART_ID_LEN - ERROR_ID.length());
            if (readBytes < MSG_PART_ID_LEN) {
                throw new HostMessageFormatException("Invalid message part. No ID");
            }
            /* It is now safe to get a part constituents */
            String partId = new String(partIDBytes, 0, MSG_PART_ID_LEN,
                    HostCodec.HEADER_CODE_PAGE).trim();
            setPartID(partId);
            int contentLen = getContentLength(stream);
            setContent(stream, contentLen);
            
        } catch (IOException e) {
            throw new HostMessageFormatException(e);
        }
        
    }

    /**
     * Create an exception using an error text returned from the host.
     * <p/>
     * Error reports are formatted like so:
     * <pre>
     * LSOKERR0 some text
     * </pre>
     * @param src the byte array originating from host
     * @param srcPos the start position in the byte array pointing to LSOKERR0 eye catcher
     * @return an exception which message contains the translated error text from the host
     */
    public HostMessageFormatException getHostException(
            final byte[] src, final int srcPos) {
        try {
            int pos = srcPos + ERROR_ID.length() + 1;
            String errorText = new String(src, pos, src.length - pos,
                    HostCodec.HEADER_CODE_PAGE).trim();
            return new HostMessageFormatException(errorText);
        } catch (UnsupportedEncodingException e) {
            return new HostMessageFormatException(e);
        }
    }
    
    /**
     * Create an exception using an error text returned from the host.
     * <p/>
     * Error reports are formatted like so:
     * <pre>
     * LSOKERR0 some text
     * </pre>
     * @param stream the stream of host data
     * @return an exception which message contains the translated error text from the host
     */
    public HostMessageFormatException getHostException(
            final InputStream stream) {
        try {
            /* Consider the message to be available */
            byte[] msgBytes = new byte[stream.available()];
            stream.read(msgBytes);
            String errorText = new String(msgBytes, 0, msgBytes.length,
                    HostCodec.HEADER_CODE_PAGE).trim();
            return new HostMessageFormatException(errorText);
        } catch (UnsupportedEncodingException e) {
            return new HostMessageFormatException(e);
        } catch (IOException e) {
            return new HostMessageFormatException(e);
        }
    }
    
    /**
     * Get the content length from a location in a byte array originating from
     * the host.
     * @param src the byte array
     * @param srcPos the current position in the byte array
     * @return the content length extracted from the byte array
     * @throws HostMessageFormatException if content length is invalid (negative)
     */
    public int getContentLength(
            final byte[] src, final int srcPos) throws HostMessageFormatException {
        ByteBuffer bb = ByteBuffer.allocate(CONTENT_LEN_LEN);
        bb.put(src, srcPos, CONTENT_LEN_LEN);
        bb.flip();
        int contentLen = bb.getInt();
        if (contentLen < 0 || contentLen > MAX_CONTENT_LEN) {
            throw new HostMessageFormatException(
            "Invalid message part content length " + contentLen);
        }
        return contentLen;
    }

    /**
     * Get the content length from a location in a stream originating from
     * the host.
     * @param stream the stream of host data
     * @return the content length extracted from the byte array
     * @throws HostMessageFormatException if content length is invalid (negative)
     */
    public int getContentLength(
            final InputStream stream) throws HostMessageFormatException {
        try {
            byte[] clBytes = new byte[CONTENT_LEN_LEN];
            int readBytes = stream.read(clBytes);
            if (readBytes < CONTENT_LEN_LEN) {
                throw new HostMessageFormatException(
                        "Invalid message part content length ");
            }
            ByteBuffer bb = ByteBuffer.allocate(CONTENT_LEN_LEN);
            bb.put(clBytes, 0, CONTENT_LEN_LEN);
            bb.flip();
            int contentLen = bb.getInt();
            if (contentLen < 0 || contentLen > MAX_CONTENT_LEN) {
                throw new HostMessageFormatException(
                "Invalid message part content length " + contentLen);
            }
            return contentLen;
        } catch (IOException e) {
            throw new HostMessageFormatException(e);
        }
    }
    /**
     * Get the content from a location in a byte array originating from
     * the host.
     * @param src the byte array
     * @param srcPos the current position in the byte array
     * @param contentLen the content length extracted from the byte array
     * @return the new position in the byte array
     * @throws HostMessageFormatException if content is invalid
     */
    public int setContent(
            final byte[] src,
            final int srcPos,
            final int contentLen) throws HostMessageFormatException {
        if (contentLen == 0) {
            setContent(null);
        } else {
            byte[] content = new byte[contentLen];
            System.arraycopy(src, srcPos, content, 0, contentLen);
            setContent(content);
        }
        return srcPos + contentLen; 
    }

    /**
     * Get the content from a location in a stream originating from
     * the host.
     * This can be a large piece of data so expect chunking.
     * @param stream the stream of host data
     * @param contentLen the content length extracted from the byte array
     * @throws HostMessageFormatException if content is invalid
     */
    public void setContent(
            final InputStream stream,
            final int contentLen) throws HostMessageFormatException {
        try {
            if (contentLen == 0) {
                setContent(null);
            } else {
                byte[] content = new byte[contentLen];
                int readBytes = 0;
                while (readBytes < contentLen) {
                    readBytes += stream.read(
                            content, readBytes, contentLen - readBytes);
                }
                setContent(content);
            }
        } catch (IOException e) {
            throw new HostMessageFormatException(e);
        }
    }
    /**
     * Available to class inheriting from this one in case they need a
     * generic print capability.
     * @return a string representation of this message part
     * 
     */
    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer(80);
        sb.append(this.getClass().getSimpleName());
        sb.append("{this=").append(Integer.toHexString(
                System.identityHashCode(this)));
        sb.append(", id=").append(getPartID());
        sb.append(", content=[").append(HostData.toHexString(getContent(), 10));
        sb.append("]}");
        return sb.toString();                        
    }

    /**
     * Two message parts are equal if they have same ID and same content.
     * @param obj message part to compare to
     * @return true if message part compared to has same id and content.
     */
    public boolean equals(final Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof LegStarMessagePart)) {
            return false;
        }
        LegStarMessagePart msgPart = (LegStarMessagePart) obj;
        if (!msgPart.getPartID().equals(getPartID())) {
            return false;
        }
        if (!Arrays.equals(msgPart.getContent(), getContent())) {
            return false;
        }
        return true;
    }

    /**
     * @see Object#hashCode() 
     * {@inheritDoc}
     */
    public int hashCode() {
        return getPartID().hashCode() + Arrays.hashCode(getContent());
    }

    /**
     * @return the amount of actual data in this message part. This allows
     * message part content to be larger than the actual payload. Payload can
     * dynamically vary for variable size structures.
     */
    public int getPayloadSize() {
        if (mPayloadSizeSet) {
            return mPayloadSize;
        }
        if (mContent != null) {
            return mContent.length;
        }
        return 0;
    }

    /**
     * @param payloadSize the payload size to set. Caller should set payload
     * size if it is different from the content length. It cannot exceed the
     * content length though.
     */
    public void setPayloadSize(final int payloadSize) {
        if (mContent == null || payloadSize > mContent.length) {
            throw new IllegalArgumentException(
            "Payload size cannot exceed content length");
        }
        mPayloadSize = payloadSize;
        mPayloadSizeSet = true;
    }

    /**
     * Checks if a payload originating from a mainframe starts with a 
     * LegStarMessagePart for a given part ID.
     * @param payload the payload to check
     * @param partID the part ID we are looking for
     * @return true if the payload starts with a LegStarMessagePart for the given part ID
     * @throws UnsupportedEncodingException if unable to read payload data
     */
    public static boolean isLegStarMessagePart(
            final byte[] payload, final String partID) throws UnsupportedEncodingException {
        if (payload == null || payload.length < MSG_PART_ID_LEN) {
            return false;
        }
        String id = new String(payload, 0, MSG_PART_ID_LEN,
                HostCodec.HEADER_CODE_PAGE).trim();
        if (id.trim().equals(partID)) {
            return true;
        }
        return false;
    }
    
}
