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

import java.util.List;

/** @deprecated */
public class Message extends LegStarMessage {

    /**
     * 
     */
    private static final long serialVersionUID = 559969254269260584L;

    /**
     * Creates an empty message.
     * @throws HeaderPartException if host encoding is wrong
     */
    public Message() throws HeaderPartException {
        super();
    }

    /**
     * Construct a message from its message parts.
     * @param headerPart the header message part
     * @param dataParts the data message parts
     */
    public Message(
            final LegStarHeaderPart headerPart,
            final List < LegStarMessagePart > dataParts) {
        super(headerPart, dataParts);
    }

}
