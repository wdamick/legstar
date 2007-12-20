/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.pool.manager;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.messaging.Address;
import com.legstar.messaging.ConnectionFactory;

/**
 * The connection pool manager holds a map of active connection pools, one
 * per address where and address identifies a target host and a specific set
 * of users credentials.
 * In order to share the same pool, 2 requests must use the same host address.
 */
public class ConnectionPoolManager {
	
	
	/** General configuration hierarchy. */
	private HierarchicalConfiguration mGeneralConfig;
	
	/** Configuration key giving the host connections pool size per endpoint.*/
	private static final String CONN_POOL_SIZE_CFG = "hostConnectionPoolSize";
	
	/** If no pool size found in configurqtion, use this default. */
	private static final int DEFAULT_POOL_SIZE = 5;
	
	/** The map of active pools in the system. */
	private Map < Address, ConnectionPool > mPools;

	/**
	 * Create a new pool manager.
	 * @param generalConfig the general configuration
	 */
	public ConnectionPoolManager(
			final HierarchicalConfiguration generalConfig) {
		mGeneralConfig = generalConfig;
		mPools = new HashMap < Address, ConnectionPool >();
	}
	
	/**
	 * Return a pool matching the request criteria.
	 * @param address the host address to match
	 * @param createIfNotFound if pool not found, one will be created
	 * @return the pool matching the criteria
	 * @throws ConnectionPoolException if pool cannot be created
	 */
	public final ConnectionPool getPool(
			final Address address,
			final boolean createIfNotFound) throws ConnectionPoolException {
		ConnectionPool  pool = mPools.get(address);
		if (pool == null && createIfNotFound) {
			pool = createConnectionPool(address);
			mPools.put(address, pool);
		}
		return pool;
	}
	
	/**
	 * Create a new connection pool for a given address. The address gives
	 * ane endpoint for which configuration data should exist.
	 * @param address the pôol common target host address
	 * @return the new connection pool
	 * @throws ConnectionPoolException in connection pool cannot be created
	 */
	private ConnectionPool createConnectionPool(
			final Address address) throws ConnectionPoolException  {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadAddressConfiguration(mGeneralConfig, address);
			ConnectionFactory connectionFactory =
				Config.loadConnectionFactory(endpointConfig);
			ConnectionPool connectionPool = new ConnectionPool(
					endpointConfig.getInt(
							CONN_POOL_SIZE_CFG, DEFAULT_POOL_SIZE),
					address, connectionFactory);
			return connectionPool;
		} catch (ConfigurationException e) {
			throw new ConnectionPoolException(e);
		}
	}

	/**
	 * Propagate shutdown on every managed pool.
	 */
	public final void shutDown() {
		Iterator < Map.Entry < Address, ConnectionPool > > entries	=
			mPools.entrySet().iterator();
		while (entries.hasNext()) {
			entries.next().getValue().shutDown();
		}
	}

	/**
	 * @return the active pools map
	 */
	public final Map < Address, ConnectionPool > getPools() {
		return mPools;
	}

	/**
	 * @param pools the active pools map to set
	 */
	public final void setPools(final Map < Address, ConnectionPool > pools) {
		mPools = pools;
	}

}
