package com.legstar.mq.client;

import com.legstar.test.client.AbstractConnectionPooledMeteringTest;

/**
 * Test WMQ transport with CICS MQ Bridge and pooling engine.
 *
 */
public class MqcihPooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /**
     * Construct with pooled endpoint.
     */
    public MqcihPooledMeteringTest() {
        super("CICSTS23-MQCIH-POOLED");
    }

}
