package com.legstar.messaging;

/**
 * Abstract factory shields users from the transport layer.
 */
public interface ConnectionFactory {
	
	/**
	 * Create a new host connection. This method creates a connection object
	 * but wether or not a physical connection is established is left to
	 * the actual implementation.
	 * 
	 * @param connectionID an identifier for this connection
	 * @param address the host address to connect to
	 * @return the new host connection
	 * @throws ConnectionException if failed to create connection
	 */
	Connection createConnection(
			String connectionID, Address address) throws ConnectionException;
}
