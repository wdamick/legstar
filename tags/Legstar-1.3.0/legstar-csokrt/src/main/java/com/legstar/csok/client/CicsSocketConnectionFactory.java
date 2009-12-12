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
package com.legstar.csok.client;

import com.legstar.messaging.AbstractConnectionFactory;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;

/**
 * A concrete connection factory for CICS Socket connections.
 */
public class CicsSocketConnectionFactory extends AbstractConnectionFactory {

    /** {@inheritDoc} */
    public HostEndpoint createEndpoint() {
        return new CicsSocketEndpoint(this);
    }

    /** {@inheritDoc} */
    public HostEndpoint getClone(final HostEndpoint endpoint) {
        return new CicsSocketEndpoint((CicsSocketEndpoint) endpoint);
    }

    /** {@inheritDoc} */
    public LegStarConnection createConnection(
            final String connectionID,
            final HostEndpoint endpoint)
    throws ConnectionException {
        return new CicsSocket(connectionID, (CicsSocketEndpoint) endpoint);
    }
}
