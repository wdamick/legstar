/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.messaging;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.SequenceInputStream;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.Arrays;

import com.legstar.codec.HostCodec;
import com.legstar.util.Util;

/**
 * MessageParts are generic named, binary containers used to send
 * and receive from host.
 */
public class LegStarMessagePart implements Serializable {
	
	/** Serial version ID.  */
	private static final long serialVersionUID = -2361247952255347371L;

	/** The message part identifier. */
	private String mID;
	
	/** The message content. */
	private byte[] mContent;
	
	/** Size of message part identifier on host (bytes). */
	private static final int MSG_PART_ID_LEN = 16;
	
	/** Size of content length on host (bytes). */
	private static final int CONTENT_LEN_LEN = 4;
	
	/** The error identifier that the host will send back.*/
	private static final String ERROR_ID = "LSOKERR0";
	
	/** Error messages sent back from host maximum size.*/
	private static final int MAX_ERROR_MSG_LEN = 266;
	
	/**
	 * Create an empty message part.
	 */
	public LegStarMessagePart() {
	}

	/**
	 * Create a named message part from a content.
	 * @param id the message part identifier
	 * @param content a binary content
	 */
	public LegStarMessagePart(final String id, final byte[] content) {
		mID = id;
		mContent = content;
	}

	/**
	 * @return the message content
	 */
	public final byte[] getContent() {
		return mContent;
	}

	/**
	 * @param content the message content to set
	 */
	public final void setContent(final byte[] content) {
		mContent = content;
	}

	/**
	 * @return the message part identifier
	 */
	public final String getID() {
		return mID;
	}

	/**
	 * @param id the message part identifier to set
	 */
	public final void setID(final String id) {
		mID = id;
	}
	
	/**
	 * Provides an input stream to serialize this message part for
	 * transmission to host.
	 * @return an input stream
	 * @throws UnsupportedEncodingException if conversion fails
	 */
	public final InputStream sendToHost() throws UnsupportedEncodingException {
		byte[] headerBytes =
			new byte[MSG_PART_ID_LEN + CONTENT_LEN_LEN];
		int pos = 0;
		HostCodec.toHostBytes(mID, headerBytes, pos,
				MSG_PART_ID_LEN, HostCodec.HEADER_CODE_PAGE);
		pos += MSG_PART_ID_LEN;
		ByteBuffer bb = ByteBuffer.allocate(4);
		bb.putInt((mContent == null)
				? 0 : mContent.length);
		bb.flip();
		bb.get(headerBytes, pos, CONTENT_LEN_LEN);
		pos += CONTENT_LEN_LEN;
		ByteArrayInputStream headerStream =
			new ByteArrayInputStream(headerBytes);
		if (mContent != null && mContent.length > 0) {
			ByteArrayInputStream contentStream =
				new ByteArrayInputStream(mContent);
			return new SequenceInputStream(headerStream, contentStream);
		} else {
			return headerStream;
		}
	}
	
	/**
	 * Recreates the message part header and content from a host byte stream.
	 * @param hostStream the host byte stream
	 * @throws HostReceiveException if creation fails
	 */
	public final void recvFromHost(final InputStream hostStream)
			throws HostReceiveException {
		
		try {
			/* First get the message part ID */
			mID = recvMsgPartID(hostStream);
			
			/* Next is the message part content length */
			byte[] clBytes = new byte[CONTENT_LEN_LEN];
			int br = hostStream.read(clBytes);
			if (br != CONTENT_LEN_LEN) {
				throw new HostReceiveException(
						"Invalid message part. No content length");
			}
			ByteBuffer bb = ByteBuffer.allocate(4);
			bb.put(clBytes, 0, CONTENT_LEN_LEN);
			bb.flip();
			int contentLen = bb.getInt();
			if (contentLen < 0) {
				throw new HostReceiveException(
						"Invalid message part content length");
			}
			
			/* Now recover the content */
			if (contentLen == 0) {
				mContent = null;
			} else {
				mContent = new byte[contentLen];
				int recvd = 0;
				try {
					while (recvd < contentLen) {
						recvd += hostStream.read(
								mContent, recvd, contentLen - recvd);
					}
				} catch (IOException e) {
					throw (new HostReceiveException(e));
				}
			}
		} catch (UnsupportedEncodingException e) {
			throw new HostReceiveException(e);
		} catch (IOException e) {
			throw new HostReceiveException(e);
		}
	}
	
	/**
	 * Everything coming back from the host must start with a message part ID.
	 * This method recovers that ID and checks that it is not an error reply.
	 * @param hostStream the host byte stream
	 * @return the message part ID
	 * @throws HostReceiveException if creation fails
	 */
	private String recvMsgPartID(final InputStream hostStream)
			throws HostReceiveException {
		
		String id;
		try {
			byte[] idBytes = new byte[MSG_PART_ID_LEN];
			int br = hostStream.read(idBytes);
			if (br < MSG_PART_ID_LEN) {
				throw new HostReceiveException(
						"Invalid message part. No ID");
			}
			id = new String(idBytes, 0, MSG_PART_ID_LEN,
					HostCodec.HEADER_CODE_PAGE).trim();
			
			/* Check if this is an error reply rather than a valid message
			 * part. In case of an error, recover the text of the error
			 * message to use as the exception description.            */
			if (ERROR_ID.compareTo(id.substring(0, ERROR_ID.length())) == 0) {
				byte[] errorTextBytes = new byte[MAX_ERROR_MSG_LEN];
				br = hostStream.read(errorTextBytes);
				String errorText = new String(errorTextBytes, 0, br,
						HostCodec.HEADER_CODE_PAGE).trim();
				/* The first 7 characters or the error message text were
				 * actually read as part of the message part ID. This is
				 * because message part IDs are 16 char long when error
				 * identifiers are 8 characters followed by space.     */
				errorText = id.substring(ERROR_ID.length() + 1,
						MSG_PART_ID_LEN) + errorText;
				throw new HostReceiveException(errorText);
			}
		} catch (IOException e) {
			throw new HostReceiveException(e);
		}
		
		return id;
	}
	
	/**
	 * @return the size in bytes of this message part host serialization
	 */
	public final int getHostSize() {
		return (MSG_PART_ID_LEN + CONTENT_LEN_LEN + ((mContent == null)
		? 0 : mContent.length));
	}

	/**
	 * Available to class inheriting from this one in case they need a
	 * generic print capability.
	 * @return a string representation of this 
	 */
	public String toString() {
        StringBuffer sb = new StringBuffer(80);
        sb.append(this.getClass().getSimpleName());
        sb.append("{this=").append(Integer.toHexString(
        		System.identityHashCode(this)));
        sb.append(", id=").append(getID());
		sb.append(", content=[").append(Util.toHexString(getContent(), 10));
        sb.append("]}");
        return sb.toString();                        
	}
	
	/**
	 * Two message parts are equal if they have same ID and same content.
	 * @param obj message part to compare to
	 * @return true if message part compared to has same id and content.
	 */
	public final boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
        if (!(obj instanceof LegStarMessagePart)) {
			return false;
		}
        LegStarMessagePart msgPart = (LegStarMessagePart) obj;
        if (!msgPart.getID().equals(getID())) {
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
    public final int hashCode() {
        return getID().hashCode() + Arrays.hashCode(getContent());
    }

	
}
