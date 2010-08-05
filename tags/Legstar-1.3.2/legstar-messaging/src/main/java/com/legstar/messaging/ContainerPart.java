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


/**
 * MessagePart representing the content of a CICS CONTAINER.
 */
public class ContainerPart extends LegStarMessagePart {

    /** Serial version ID. */
    private static final long serialVersionUID = -2440806472909705571L;

    /**
     * Create an empty container.
     * @param id name of the container
     */
    public ContainerPart(final String id) {
        super(id, null);
    }

    /**
     * Create a container from a binary content.
     * @param id name of the container
     * @param content binary content
     */
    public ContainerPart(final String id, final byte[] content) {
        super(id, content);
    }

}
