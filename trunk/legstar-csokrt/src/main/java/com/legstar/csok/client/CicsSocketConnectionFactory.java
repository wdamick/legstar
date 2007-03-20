package com.legstar.csok.client;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.messaging.Address;
import com.legstar.messaging.Connection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;

/**
 * A concrete connection factory for CICS Socket connections.
 */
public class CicsSocketConnectionFactory implements ConnectionFactory {

	/** Configuration XPath location for socket connect timeout. */
	private static final String SOK_CONNECT_TIMEOUT_CFG =
		"socketConnectTimeout";
	
	/** Configuration XPath location for socket receive timeout. */
	private static final String SOK_RECEIVE_TIMEOUT_CFG =
		"socketReceiveTimeout";
	
	/** Configuration for an endpoint.	 */
	private HierarchicalConfiguration mEndpointConfig;
	
	/**
	 * Constructor for the CICS Socket factory.
	 * @param endpointConfig the an XML sub-hierarchy for an endpoint
	 */
	public CicsSocketConnectionFactory(
			final HierarchicalConfiguration endpointConfig) {
		mEndpointConfig = endpointConfig;
		mEndpointConfig.setExpressionEngine(new XPathExpressionEngine());
	}
	
	/** (non-Javadoc).
	 * @see com.legstar.messaging.ConnectionFactory#createConnection(
	 * java.lang.String, com.legstar.messaging.Address)
	 * {@inheritDoc}
	 */
	public final Connection createConnection(
			final String connectionID,
			final Address address) throws ConnectionException {
		
		CicsSocket connection = new CicsSocket(
				connectionID, createEndPoint(address, mEndpointConfig));
		
		/* If configuration specifies timeout values, use them for connection
		 * rather than the default values */
		long timeout = mEndpointConfig.getLong(SOK_CONNECT_TIMEOUT_CFG, 0L);
		if (timeout > 0L) {
			connection.setConnectTimeout(timeout);
		}
		timeout = mEndpointConfig.getLong(SOK_RECEIVE_TIMEOUT_CFG, 0L);
		if (timeout > 0L) {
			connection.setReceiveTimeout(timeout);
		}
		
		return connection;
	}

	/**
	 * Create a CICS Socket endpoint by merging parameters from configuration
	 * with parameters requested by the client in Address.
	 * @param address the client endpoint parameters
	 * @param endpointConfig the endpoint configuration
	 * @return a CICS Socket endpoint
	 * @throws CicsSocketConnectionException if endpoint cannot be constructed
	 */
	private CicsSocketEndpoint createEndPoint(
			final Address address,
			final HierarchicalConfiguration endpointConfig)
			throws CicsSocketConnectionException {
		CicsSocketEndpoint cicsSocketEndpoint =
			new CicsSocketEndpoint(endpointConfig);
		
		/* If client is providing credentials, they take precedence over
		 * the configuration parameters */
		if (address.getHostUserID() != null 
				&& address.getHostUserID().length() > 0) {
			cicsSocketEndpoint.setHostUserID(address.getHostUserID());
		}
		if (address.getHostPassword() != null 
				&& address.getHostPassword().length() > 0) {
			cicsSocketEndpoint.setHostPassword(address.getHostPassword());
		}
		if (address.getHostCharset() != null 
				&& address.getHostCharset().length() > 0) {
			cicsSocketEndpoint.setHostCharset(address.getHostCharset());
		}
		if (address.isHostTraceMode()) {
			cicsSocketEndpoint.setHostTraceMode(true);
		}
		
		/* Check that this endpoint contains all mandatory parameters */
		if (cicsSocketEndpoint.getHostCharset() == null
				|| cicsSocketEndpoint.getHostCharset().length() == 0) {
			throw new CicsSocketConnectionException(
					"No host character set has been provided.");
		}
		if (cicsSocketEndpoint.getHostUserID() == null
				|| cicsSocketEndpoint.getHostUserID().length() == 0) {
			throw new CicsSocketConnectionException(
					"No host user ID has been provided.");
		}
		if (cicsSocketEndpoint.getHostIPAddress() == null
				|| cicsSocketEndpoint.getHostIPAddress().length() == 0) {
			throw new CicsSocketConnectionException(
					"No host IP address has been provided.");
		}
		if (cicsSocketEndpoint.getHostIPPort() == 0) {
			throw new CicsSocketConnectionException(
					"No host IP port has been provided.");
		}
		
		return cicsSocketEndpoint;
	}
	
}
