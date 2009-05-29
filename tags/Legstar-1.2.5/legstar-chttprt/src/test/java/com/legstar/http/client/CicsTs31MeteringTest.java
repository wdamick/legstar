package com.legstar.http.client;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.config.Config;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.test.client.AbstractConnectionMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging.
 *
 */
public class CicsTs31MeteringTest extends AbstractConnectionMeteringTest {

    /**
     * Construct.
     * @throws ConnectionException if connection cannot be created
     */
    public CicsTs31MeteringTest() throws ConnectionException {
        super("config.xml", "CICSTS31");
    }

    /** {@inheritDoc} */
    @Override
    public LegStarConnection createConnection(
            final String configFileName,
            final String endpointName) throws ConnectionException {
        try {
            CicsHttpEndpoint endpoint = new CicsHttpEndpoint(
                    Config.loadEndpointConfiguration(configFileName, endpointName));
            endpoint.setHostTraceMode(false);
            CicsHttp connection = new CicsHttp(
                    endpointName, endpoint,
                    AbstractHttpConnectionTester.DEFAULT_CONNECT_TIMEOUT_MSEC,
                    AbstractHttpConnectionTester.DEFAULT_READ_TIMEOUT_MSEC);
            return connection;
        } catch (ConfigurationException e) {
            throw new ConnectionException(e);
        }
    }

}
