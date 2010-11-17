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

import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.Map;
import java.nio.ByteBuffer;

import com.legstar.codec.HostCodec;

/**
 * The header is a special message part which content gives the number
 * of input parts as well as a JSON-encoded string describing the action
 * that the host server should perform.
 */
public class LegStarHeaderPart extends LegStarMessagePart {

    /** Serial version ID. */
    private static final long serialVersionUID = -4021804844084093286L;

    /** The header identifier (used as an eye catcher by the CICS
     *  counterpart).*/
    public static final String HEADER_PART_ID = "LSOKHEAD";

    /** Size of data parts number on host (bytes). */
    private static final int DATAPARTS_NUM_LEN = 4;

    /** Size of Json string length on host (bytes). */
    private static final int JSON_LEN_LEN = 4;

    /** No argument constructor creates a header part with no data parts
     * and an empty json host action description. 
     * @throws HeaderPartException if header code page is not supported
     *  */
    public LegStarHeaderPart() throws HeaderPartException {
        super(HEADER_PART_ID, null);
        setContent(0, "");
    }

    /** Creates a header part for a given number of input parts and a
     * JSON string describing the expected host action.
     * and an empty json host action description. 
     * @param dataPartsNumber the number of data message parts
     * @param jsonString the host action description
     * @throws HeaderPartException if header code page is not supported
     *  */
    public LegStarHeaderPart(
            final int dataPartsNumber, final String jsonString)
    throws HeaderPartException {
        super(HEADER_PART_ID, null);
        setContent(dataPartsNumber, jsonString);
    }

    /**
     * Convenience constructor takes the host action description as
     * key/value pairs rather than a JSON string.
     * 
     * @param dataPartsNumber the number of data message parts
     * @param keyValues the protocol elements
     * @throws HeaderPartException if character set is invalid
     */
    public LegStarHeaderPart(
            final Map < String, Object > keyValues,
            final int dataPartsNumber) throws HeaderPartException {
        super(HEADER_PART_ID, null);
        /* Create the host content */
        setContent(dataPartsNumber, getJsonFromMap(keyValues));
    }

    /**
     * Creates new header part content.
     * The layout for a header part content is:
     * 4 bytes giving the number of input message parts (big endian)
     * 4 bytes giving the size of the key/value pairs (big endian)
     * A Json serialization of the key/value pairs
     * @param dataPartsNumber number of input parts
     * @param jsonString the JSON host action description
     * @throws HeaderPartException if conversion to host fails
     */
    public void setContent(
            final int dataPartsNumber,
            final String jsonString) throws HeaderPartException {

        byte[] hostJson;
        try {
            hostJson = jsonString.getBytes(HostCodec.HEADER_CODE_PAGE);
        } catch (UnsupportedEncodingException e) {
            throw new HeaderPartException(e);
        }
        setContent(
                new byte[DATAPARTS_NUM_LEN + JSON_LEN_LEN + hostJson.length]);
        int pos = 0;
        setDataPartsNumber(dataPartsNumber);
        pos += DATAPARTS_NUM_LEN;
        ByteBuffer bb = ByteBuffer.allocate(JSON_LEN_LEN);
        bb.putInt(hostJson.length);
        bb.flip();
        bb.get(getContent(), pos, JSON_LEN_LEN);
        pos += JSON_LEN_LEN;
        if (hostJson.length > 0) {
            System.arraycopy(
                    hostJson, 0, getContent(), pos, hostJson.length);
        }
    }

    /**
     * Serialize key/values in JSON compliant string.
     * @param map key/values pair
     * @return the JSON string
     */
    public static String getJsonFromMap(final Map < String, Object > map) {
        StringBuilder sb = new StringBuilder();
        boolean firstentry = true;
        sb.append("{");
        Map.Entry < String, Object > entry = null;
        Iterator < Map.Entry < String, Object > > entries
        = map.entrySet().iterator();
        while (entries.hasNext()) {
            entry = entries.next();
            /* Simple String values are serialized as "key":"value" */
            if (entry.getValue() instanceof java.lang.String) {
                if (!firstentry) {
                    sb.append(",");
                }
                sb.append("\"" + entry.getKey() + "\":\"" + entry.getValue()
                        + "\"");
            } else {
                /* String arrays are serialized as "key":["value1",.,"valuen"]*/
                if (entry.getValue() instanceof java.lang.String[]) {
                    if (!firstentry) {
                        sb.append(",");
                    }
                    sb.append("\"" + entry.getKey() + "\":[");
                    String[] array = (String[]) entry.getValue();
                    for (int i = 0; i < array.length; i++) {
                        if (i > 0) {
                            sb.append(",");
                        }
                        sb.append("\"" + array[i] + "\"");
                    }
                    sb.append("]");
                }
            }
            if (firstentry) {
                firstentry = false;
            }
        }
        sb.append("}");
        return sb.toString();
    }

    /**
     * Message parts number is extracted from the header content.
     * The message parts number occupies the four first bytes of the content.
     * @return the number of data message parts
     */
    public int getDataPartsNumber() {
        if (getContent() == null 
                || getContent().length < DATAPARTS_NUM_LEN) {
            return 0;
        }
        ByteBuffer bb = ByteBuffer.allocate(DATAPARTS_NUM_LEN);
        bb.put(getContent(), 0, DATAPARTS_NUM_LEN);
        bb.flip();
        return bb.getInt();
    }

    /**
     * @param dataPartsNumber the number of data message parts to set
     */
    public void setDataPartsNumber(final int dataPartsNumber) {
        ByteBuffer bb = ByteBuffer.allocate(DATAPARTS_NUM_LEN);
        bb.putInt(dataPartsNumber);
        bb.flip();
        bb.get(getContent(), 0, DATAPARTS_NUM_LEN);
    }

    /**
     * The json string length occupies four bytes following the number
     * of data parts.
     * @return the json host string length
     */
    public int getJsonStringLen() {
        if (getContent() == null 
                || getContent().length 
                < (DATAPARTS_NUM_LEN + JSON_LEN_LEN)) {
            return 0;
        }
        ByteBuffer bb = ByteBuffer.allocate(JSON_LEN_LEN);
        bb.put(getContent(), DATAPARTS_NUM_LEN, JSON_LEN_LEN);
        bb.flip();
        return bb.getInt();
    }

    /**
     * @return the JSON string
     * @throws HeaderPartException if host conversion fails
     */
    public String getJsonString() throws HeaderPartException {
        if (getJsonStringLen() == 0) {
            return null;
        }
        try {
            return new String(getContent(), DATAPARTS_NUM_LEN + JSON_LEN_LEN,
                    getJsonStringLen(), HostCodec.HEADER_CODE_PAGE);
        } catch (UnsupportedEncodingException e) {
            throw new HeaderPartException(e);
        }
    }

    /**
     * @param jsonString the JSON host action description
     * @throws HeaderPartException if map cannot be converted to host
     * string
     */
    public void setJsonString(
            final String jsonString)
    throws HeaderPartException {
        setContent(getDataPartsNumber(), jsonString);
    }

    /**
     * @param keyValues the key/value pairs to set
     * @throws HeaderPartException if map cannot be converted to host
     * string
     */
    public void setKeyValues(
            final Map < String, Object > keyValues)
    throws HeaderPartException {
        setContent(getDataPartsNumber(), getJsonFromMap(keyValues));
    }

    /** {@inheritDoc} */
    public String toString() {
        StringBuffer sb = new StringBuffer(80);
        sb.append(this.getClass().getSimpleName());
        sb.append("{this=").append(Integer.toHexString(
                System.identityHashCode(this)));
        sb.append(", dataPartsNumber=").append(getDataPartsNumber());
        try {
            sb.append(", jsonString=").append(getJsonString());
        } catch (HeaderPartException e) {
            sb.append(", jsonString=").append("");
        }
        sb.append("}");
        return sb.toString();                        
    }

    /**
     * Checks if a payload originating from a mainframe starts with a 
     * LegStarHeader.
     * @param payload the payload to check
     * @return true if the payload starts with a LegStarHeader
     * @throws UnsupportedEncodingException if unable to read payload data
     */
    public static boolean isLegStarHeader(
            final byte[] payload) throws UnsupportedEncodingException {
        return isLegStarMessagePart(payload, HEADER_PART_ID);
    }
}
