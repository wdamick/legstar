package com.legstar.mq.client;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.config.Config;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.test.client.AbstractConnectionMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging.
 *
 */
public class LsmsgMeteringTest extends AbstractConnectionMeteringTest {

    /**
     * Construct.
     * @throws ConnectionException if connection cannot be created
     */
    public LsmsgMeteringTest() throws ConnectionException {
        super("config.xml", "CICSTS23-LSMSG");
    }

    /** {@inheritDoc} */
    @Override
    public LegStarConnection createConnection(
            final String configFileName,
            final String endpointName) throws ConnectionException {
        try {
            CicsMQEndpoint endpoint = new CicsMQEndpoint(
                    Config.loadEndpointConfiguration(configFileName, endpointName));
            endpoint.setHostTraceMode(false);
            AbstractCicsMQ connection = new CicsMQLsmsg(
                    endpointName, endpoint,
                    AbstractMQConnectionTester.DEFAULT_CONNECT_TIMEOUT_MSEC,
                    AbstractMQConnectionTester.DEFAULT_READ_TIMEOUT_MSEC);
            return connection;
        } catch (ConfigurationException e) {
            throw new ConnectionException(e);
        }
    }

}
