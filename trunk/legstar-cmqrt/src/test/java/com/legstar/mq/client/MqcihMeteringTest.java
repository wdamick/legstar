package com.legstar.mq.client;

/**
 * Test WMQ transport with CICS MQ Bridge.
 *
 */
public class MqcihMeteringTest extends AbstractMeteringTest {

    /**
     * Construct endpoint.
     */
    public MqcihMeteringTest() {
        super("CICSTS23-MQCIH");
    }

    /** {@inheritDoc} */
    @Override
    public AbstractCicsMQ createConnection(
            final String endpointName,
            final CicsMQEndpoint legstarEndpoint) throws CicsMQConnectionException {
        AbstractCicsMQ connection = new CicsMQMqcih(
                endpointName, legstarEndpoint,
                AbstractTester.DEFAULT_CONNECT_TIMEOUT_MSEC,
                AbstractTester.DEFAULT_READ_TIMEOUT_MSEC);
        return connection;
    }

}
