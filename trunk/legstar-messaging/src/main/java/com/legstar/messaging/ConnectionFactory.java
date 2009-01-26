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
 * Abstract factory shields users from the transport layer.
 */
public interface ConnectionFactory {

    /**
     * Create a new host connection. This method creates a connection object
     * but wether or not a physical connection is established is left to
     * the actual implementation.
     * 
     * @param connectionID an identifier for this connection
     * @param address the host address to connect to
     * @return the new host connection
     * @throws ConnectionException if failed to create connection
     */
    LegStarConnection createConnection(
            String connectionID,
            LegStarAddress address) throws ConnectionException;
}
