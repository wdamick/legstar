package com.legstar.messaging;

/**
 * A mock connection factory.
 *
 */
public class MockConnectionFactory implements ConnectionFactory {

    /** {@inheritDoc}*/
    public LegStarConnection createConnection(final String connectionID,
            final LegStarAddress address, final HostEndpoint endpoint)
            throws ConnectionException {
        return null;
    }

    /** {@inheritDoc}*/
    public HostEndpoint createEndpoint() {
        return null;
    }

}
