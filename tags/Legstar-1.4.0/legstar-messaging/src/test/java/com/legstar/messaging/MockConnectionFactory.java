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

/**
 * A mock connection factory.
 *
 */
public class MockConnectionFactory implements ConnectionFactory {

    /** {@inheritDoc}*/
    public LegStarConnection createConnection(final String connectionID,
            final LegStarAddress address, final HostEndpoint endpoint)
            throws ConnectionException {
        return null;
    }

    /** {@inheritDoc}*/
    public HostEndpoint createEndpoint() {
        return null;
    }

}
