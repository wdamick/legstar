package com.legstar.csok.test;

import com.legstar.test.client.AbstractConnectionPooledMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging and pooling engine.
 *
 */
public class CicsTs23PooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /**
     * Construct with pooled endpoint.
     */
    public CicsTs23PooledMeteringTest() {
        super("CICSTS23-POOLED");
    }

}
