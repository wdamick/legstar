package com.legstar.mock.client;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;

/**
 * An endpoint for the mock transport.
 *
 */
public class MockEndpoint extends HostEndpoint {
    
    /**
     * No-arg constructor.
     */
    public MockEndpoint() {
        
    }

    /**
     * Constructor using an existing connection factory.
     * @param connectionFactory an instance of a connection factory
     */
    public MockEndpoint(final ConnectionFactory connectionFactory) {
        super(connectionFactory);
    }

    /** {@inheritDoc} */
    public void check() throws ConnectionException {
    }

}
