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

import java.util.Map;

/** @deprecated */
public class HeaderPart extends LegStarHeaderPart {

    /** Serial version ID. */
    private static final long serialVersionUID = -5049074592429902791L;

    /**
     * Default constructor.
     * @throws HeaderPartException if header part cannot be constructed
     */
    public HeaderPart() throws HeaderPartException {
        super();
    }

    /** Creates a header part for a given number of input parts and a
     * JSON string describing the expected host action.
     * and an empty json host action description. 
     * @param dataPartsNumber the number of data message parts
     * @param jsonString the host action description
     * @throws HeaderPartException if header code page is not supported
     *  */
    public HeaderPart(
            final int dataPartsNumber, final String jsonString)
    throws HeaderPartException {
        super(dataPartsNumber, jsonString);
    }

    /**
     * Convenience constructor takes the host action description as
     * key/value pairs rather than a JSON string.
     * 
     * @param dataPartsNumber the number of data message parts
     * @param keyValues the protocol elements
     * @throws HeaderPartException if character set is invalid
     */
    public HeaderPart(
            final Map < String, Object > keyValues,
            final int dataPartsNumber) throws HeaderPartException {
        super(keyValues, dataPartsNumber);
    }


}
