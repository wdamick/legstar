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


/**
 * MessagePart representing the content of a CICS commarea.
 */
public class CommareaPart extends LegStarMessagePart {

    /** Serial version ID. */
    private static final long serialVersionUID = -6186410695573533903L;

    /** The header identifier (used as an eye catcher by the CICS
     *  counterpart).*/
    public static final String COMMAREA_PART_ID = "LSOKCOMMAREA";

    /**
     * Create a commarea from a binary content.
     * @param content binary content
     */
    public CommareaPart(final byte[] content) {
        super(COMMAREA_PART_ID, content);
    }

}
