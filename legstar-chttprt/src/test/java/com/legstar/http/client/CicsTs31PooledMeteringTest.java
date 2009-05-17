package com.legstar.http.client;

import com.legstar.test.client.AbstractConnectionPooledMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging and pooling engine.
 *
 */
public class CicsTs31PooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /**
     * Construct with pooled endpoint.
     */
    public CicsTs31PooledMeteringTest() {
        super("CICSTS31-POOLED");
    }

}
