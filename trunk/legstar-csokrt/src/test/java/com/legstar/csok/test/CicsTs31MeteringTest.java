package com.legstar.csok.test;

import java.rmi.server.UID;

import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarConnection;
import com.legstar.test.client.AbstractConnectionMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging.
 *
 */
public class CicsTs31MeteringTest extends AbstractConnectionMeteringTest {

    /** {@inheritDoc} */
    @Override
    public HostEndpoint createEndpoint() {
        HostEndpoint endpoint = AbstractSocketConnectionTester.getCicsTs31Endpoint();
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
