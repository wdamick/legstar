package com.legstar.mq.client;

/**
 * Test WMQ transport with CICS MQ Bridge and pooling engine.
 *
 */
public class MqcihPooledMeteringTest extends AbstractPooledMeteringTest {

    /**
     * Construct with pooled endpoint.
     */
    public MqcihPooledMeteringTest() {
        super("CICSTS23-MQCIH-POOLED");
    }

}
