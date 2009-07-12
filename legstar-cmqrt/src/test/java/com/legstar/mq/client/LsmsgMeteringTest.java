package com.legstar.mq.client;

import java.rmi.server.UID;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarConnection;
import com.legstar.test.client.AbstractConnectionMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging.
 *
 */
public class LsmsgMeteringTest extends AbstractConnectionMeteringTest {

    /** {@inheritDoc} */
    @Override
    public HostEndpoint createEndpoint() {
        HostEndpoint endpoint = AbstractMQConnectionTester.getLsmsgEndpoint();
        endpoint.setHostTraceMode(false);
        return endpoint;
    }

    /** {@inheritDoc} */
    @Override
    public LegStarConnection createConnection() throws ConnectionException {
        AbstractCicsMQ connection = new CicsMQLsmsg(
                (new UID()).toString(), (CicsMQEndpoint) getEndpoint());
        return connection;
    }

}
