package com.legstar.mq.client;

import com.legstar.test.client.AbstractConnectionPooledMeteringTest;

/**
 * Test WMQ transport with LegStar Messaging and pooling engine.
 *
 */
public class LsmsgPooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /**
     * Construct with pooled endpoint.
     */
    public LsmsgPooledMeteringTest() {
        super("CICSTS23-LSMSG-POOLED");
    }

}
