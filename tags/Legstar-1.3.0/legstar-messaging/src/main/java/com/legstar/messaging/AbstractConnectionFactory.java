package com.legstar.messaging;


/**
 * Provides common behavior to all transport connection factories.
 *
 */
public abstract class AbstractConnectionFactory implements ConnectionFactory {

    /** 
     * {@inheritDoc}.
     * Performs the following steps:
     * <ul>
     * <li>Clone the endpoint because we are about to modify it and would like endpoints to be immutable</li>
     * <li>Enrich the clone with address related data (user, password, etc)</li>
     * <li>Check that all endpoint parameters are consistent</li>
     * </ul>
     *  */
    public LegStarConnection createConnection(
            final String connectionID,
            final LegStarAddress address,
            final HostEndpoint endpoint) throws ConnectionException {
        HostEndpoint cloneEndpoint = getClone(endpoint);
        cloneEndpoint.enrich(address);
        cloneEndpoint.checkAll();
        return createConnection(connectionID, cloneEndpoint);
    }
    
    /**
     * Creates a clone of the endpoint parameter.
     * @param endpoint the endpoint to clone
     * @return a clone
     */
    public abstract HostEndpoint getClone(final HostEndpoint endpoint);
    
    /**
     * Create a connection.
     * @param connectionID the connection identifier
     * @param endpoint the host endpoint
     * @return a connection to the host
     * @throws ConnectionException if something goes wrong
     */
    public abstract LegStarConnection createConnection(
            final String connectionID,
            final HostEndpoint endpoint) throws ConnectionException;
}
