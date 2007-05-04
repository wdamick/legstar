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

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.nio.ByteBuffer;


/**
 * The header is a special message part which content gives the number
 * of input parts as well as a variable number of key/value pairs that
 * the client wishes to communicate to the host server.
 */
public class HeaderPart extends MessagePart {
	
	/** The header identifier (used as an eye catcher by the CICS
	 *  counterpart).*/
	public static final String HEADER_PART_ID = "LSOKHEAD";
	
	/** Key/Value pairs. */
	private Map < String, String > mKeyValues;
	
	/** Key/Values serialized as a Json stream. */
	private String mStringizedKeyValues;
	
	/** Number of data message parts. */
	private int mDataPartsNumber;

	/**
	 * Contructor builds the internal content structure in host character set.
	 * The final content starts with a four bytes integer giving the number
	 * of input message parts. The rest of the content is
	 * a variable length JSON serialization of key/value pairs describing
	 * what the host server should do. The JSON string is preceded by a
	 * 4 bytes int giving its size.
	 * 
	 * @param keyValues the protocol elements
	 * @param dataPartsNumber the number of data message parts
	 * @param hostCharset the host character set
	 * @throws UnsupportedEncodingException if character set is invalid
	 */
	public HeaderPart(
			final Map < String, String > keyValues,
			final int dataPartsNumber,
			final String hostCharset) throws UnsupportedEncodingException {
		super(HEADER_PART_ID, null);
		mKeyValues = keyValues;
		mDataPartsNumber = dataPartsNumber;
		
		/* Stuff the content with :
		 * 4 bytes giving the number of input message parts (big endian)
		 * 4 bytes giving the size of the key/value pairs (big endian)
		 * A Json serialization of the key/value pairs
		 *  */
		mStringizedKeyValues = stringizeKeyValues(keyValues);
		byte[] hostKeyValues = mStringizedKeyValues.getBytes(hostCharset);
		setContent(new byte[8 + hostKeyValues.length]);
		int pos = 0;
		ByteBuffer bb = ByteBuffer.allocate(4);
		bb.putInt(dataPartsNumber);
		bb.flip();
		bb.get(getContent(), pos, 4);
		pos += 4;
		bb.clear();
		bb.putInt(hostKeyValues.length);
		bb.flip();
		bb.get(getContent(), pos, 4);
		pos += 4;
		System.arraycopy(
				hostKeyValues, 0, getContent(), pos, hostKeyValues.length);
	}
	
	/**
	 * Construct a header part from a message part. In this version, we
	 * do not convert the JSON string to a key/value pair map. 
	 * @param messagePart an existing message part
	 * @throws HeaderPartException if message part is not a valid header part
	 *   */
	public HeaderPart(
			final MessagePart messagePart) throws HeaderPartException {
		super(messagePart.getID(), messagePart.getContent());
		
		/* First check that the ID matches */
		if (messagePart.getID().compareTo(HEADER_PART_ID) != 0) {
			throw new HeaderPartException(
					"Message part is not a header part.");
		}
		
		/* Get the number of data parts as the firs 4 bytes of the content */
		ByteBuffer bb = ByteBuffer.allocate(4);
		bb.put(messagePart.getContent(), 0, 4);
		bb.flip();
		mDataPartsNumber = bb.getInt();
		
		/** TODO key/value pairs should be reconstructed from JSON string */
		mKeyValues = new HashMap < String, String >();
	}
	
	/** No argument constructor. */
	public HeaderPart() {
		super(HEADER_PART_ID, null);
		mDataPartsNumber = 0;
		mKeyValues = new HashMap < String, String >();
	}
	
	/**
	 * Serialize key/values in JSON compliant string.
	 * @param map key/values pair
	 * @return the JSON string
	 */
	public static String stringizeKeyValues(final Map < String, String > map) {
		StringBuilder sb = new StringBuilder();
		boolean firstentry = true;
		sb.append("{");
		Map.Entry < String, String > entry = null;
		Iterator < Map.Entry < String, String > > entries
					= map.entrySet().iterator();
		while (entries.hasNext()) {
			if (firstentry) {
				firstentry = false;
			} else {
				sb.append(",");
			}
			entry = entries.next();
			sb.append("\"" + entry.getKey() + "\":\"" + entry.getValue()
					+ "\"");
		}
		sb.append("}");
		return sb.toString();
	}
	
	/**
	 * @return the Key/Values serialized as a Json stream
	 */
	public final String getStringizedKeyValues() {
		return mStringizedKeyValues;
	}

	/**
	 * @return the number of data message parts
	 */
	public final int getDataPartsNumber() {
		return mDataPartsNumber;
	}

	/**
	 * @param dataPartsNumber the number of data message parts to set
	 */
	public final void setDataPartsNumber(final int dataPartsNumber) {
		mDataPartsNumber = dataPartsNumber;
	}

	/**
	 * @return the key/value pairs
	 */
	public final Map < String, String > getKeyValues() {
		return mKeyValues;
	}

	/**
	 * @param keyValues the key/value pairs to set
	 */
	public final void setKeyValues(final Map < String, String > keyValues) {
		mKeyValues = keyValues;
	}


}
