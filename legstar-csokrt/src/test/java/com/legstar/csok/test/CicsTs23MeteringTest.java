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
package com.legstar.csok.test;

import java.rmi.server.UID;

import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarConnection;
import com.legstar.test.connection.client.AbstractConnectionMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging.
 *
 */
public class CicsTs23MeteringTest extends AbstractConnectionMeteringTest {

    /** {@inheritDoc} */
    @Override
    public HostEndpoint createEndpoint() {
        HostEndpoint endpoint = AbstractSocketConnectionTester.getCicsTs23Endpoint();
        endpoint.setHostTraceMode(false);
        return endpoint;
    }

    /** {@inheritDoc} */
    @Override
    public LegStarConnection createConnection() throws ConnectionException {
        CicsSocket connection = new CicsSocket(
                (new UID()).toString(), (CicsSocketEndpoint) getEndpoint());
        return connection;
    }
}
