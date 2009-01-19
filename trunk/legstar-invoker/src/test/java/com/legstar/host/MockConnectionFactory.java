package com.legstar.host;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;

/**
 * Simulates a transport factory.
 *
 */
public class MockConnectionFactory implements ConnectionFactory {

    /** Configuration for an endpoint. */
    private HierarchicalConfiguration mEndpointConfig;

    /**
     * Constructor for the transport factory.
     * @param endpointConfig the an XML sub-hierarchy for an endpoint
     */
    public MockConnectionFactory(
            final HierarchicalConfiguration endpointConfig) {
        mEndpointConfig = endpointConfig;
        mEndpointConfig.setExpressionEngine(new XPathExpressionEngine());
    }

    /** {@inheritDoc} */
    public LegStarConnection createConnection(final String connectionID,
            final LegStarAddress address) throws ConnectionException {
        return new MockConnection();
    }

}
