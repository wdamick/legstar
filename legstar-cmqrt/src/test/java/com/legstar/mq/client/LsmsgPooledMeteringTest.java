package com.legstar.mq.client;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.test.client.AbstractConnectionPooledMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging and pooling engine.
 *
 */
public class LsmsgPooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /** {@inheritDoc} */
    public PoolingEngineConfig getPoolingEngineConfig() {
        return AbstractMQConnectionTester.getLsmsgPoolingEngineConfig();
    }

}
