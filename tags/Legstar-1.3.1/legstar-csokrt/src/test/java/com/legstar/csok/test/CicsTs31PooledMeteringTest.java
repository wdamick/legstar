package com.legstar.csok.test;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.test.connection.client.AbstractConnectionPooledMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging and pooling engine.
 *
 */
public class CicsTs31PooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /** {@inheritDoc} */
    public PoolingEngineConfig getPoolingEngineConfig() {
        return AbstractSocketConnectionTester.getCicsTs31PoolingEngineConfig();
    }

}
