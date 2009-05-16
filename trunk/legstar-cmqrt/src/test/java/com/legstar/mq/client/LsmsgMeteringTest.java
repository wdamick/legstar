package com.legstar.mq.client;

/**
 * Test WMQ transport with LegStar Messaging.
 *
 */
public class LsmsgMeteringTest extends AbstractMeteringTest {

    /**
     * Construct endpoint.
     */
    public LsmsgMeteringTest() {
        super("CICSTS23-LSMSG");
    }

    /** {@inheritDoc} */
    @Override
    public AbstractCicsMQ createConnection(
            final String endpointName,
            final CicsMQEndpoint legstarEndpoint) throws CicsMQConnectionException {
        AbstractCicsMQ connection = new CicsMQLsmsg(
                endpointName, legstarEndpoint,
                AbstractTester.DEFAULT_CONNECT_TIMEOUT_MSEC,
                AbstractTester.DEFAULT_READ_TIMEOUT_MSEC);
        return connection;
    }

}
