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
