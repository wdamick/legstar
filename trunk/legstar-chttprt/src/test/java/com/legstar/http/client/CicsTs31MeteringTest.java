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
package com.legstar.http.client;

import java.rmi.server.UID;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarConnection;
import com.legstar.test.connection.client.AbstractConnectionMeteringTest;

/**
 * Test HTTP transport with LegStar Messaging.
 *
 */
public class CicsTs31MeteringTest extends AbstractConnectionMeteringTest {

    /** {@inheritDoc} */
    @Override
    public HostEndpoint createEndpoint() {
        HostEndpoint endpoint = AbstractHttpConnectionTester.getCicsTs31Endpoint();
        endpoint.setHostTraceMode(false);
        return endpoint;
    }

    /** {@inheritDoc} */
    @Override
    public LegStarConnection createConnection() throws ConnectionException {
        CicsHttp connection = new CicsHttp(
                (new UID()).toString(), (CicsHttpEndpoint) getEndpoint());
        return connection;
    }

}
