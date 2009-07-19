package com.legstar.mq.client;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.test.connection.client.AbstractConnectionPooledMeteringTest;

/**
 * Test WMQ transport with CICS MQ Bridge and pooling engine.
 *
 */
public class MqcihPooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /** {@inheritDoc} */
    public PoolingEngineConfig getPoolingEngineConfig() {
        return AbstractMQConnectionTester.getMqcihPoolingEngineConfig();
    }

}
