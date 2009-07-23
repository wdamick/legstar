package com.legstar.http.client;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.test.connection.client.AbstractConnectionPooledMeteringTest;

/**
 * Test HTTP transport with LegStar Messaging and pooling engine.
 *
 */
public class CicsTs23PooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /** {@inheritDoc} */
    public PoolingEngineConfig getPoolingEngineConfig() {
        return AbstractHttpConnectionTester.getCicsTs23PoolingEngineConfig();
    }

}
