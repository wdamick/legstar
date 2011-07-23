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
package com.legstar.mock.client;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;

/**
 * An endpoint for the mock transport.
 *
 */
public class MockEndpoint extends HostEndpoint {
    
    /**
     * No-arg constructor.
     */
    public MockEndpoint() {
        
    }

    /**
     * Constructor using an existing connection factory.
     * @param connectionFactory an instance of a connection factory
     */
    public MockEndpoint(final ConnectionFactory connectionFactory) {
        super(connectionFactory);
    }

    /** {@inheritDoc} */
    public void check() throws ConnectionException {
    }

}
