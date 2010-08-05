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
package com.legstar.mock.client;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;

/**
 * Simulates a transport factory.
 *
 */
public class MockConnectionFactory implements ConnectionFactory {

    /**
     * Constructor for the transport factory.
     */
    public MockConnectionFactory() {
    }

    /** {@inheritDoc} */
    public HostEndpoint createEndpoint() {
        return new MockEndpoint(this);
    }

    /** {@inheritDoc} */
    public LegStarConnection createConnection(
            final String connectionID,
            final LegStarAddress address,
            final HostEndpoint endpoint) throws ConnectionException {
        return new MockConnection(connectionID);
    }

}
