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


/** @deprecated */
public class MessagePart extends LegStarMessagePart {

    /**
     * 
     */
    private static final long serialVersionUID = 5672052862054658459L;

    /**
     * Create an empty message part.
     */
    public MessagePart() {
        super();
    }

    /**
     * Create a named message part from a content.
     * @param id the message part identifier
     * @param content a binary content
     */
    public MessagePart(final String id, final byte[] content) {
        super(id, content);
    }

}
