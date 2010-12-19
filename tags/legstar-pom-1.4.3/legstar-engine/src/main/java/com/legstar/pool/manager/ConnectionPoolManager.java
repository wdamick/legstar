/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.pool.manager;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;

/**
 * The connection pool manager holds a map of active connection pools, one
 * per address where and address identifies a target host and a specific set
 * of users credentials.
 * In order to share the same pool, 2 requests must use the same host address.
 */
public class ConnectionPoolManager {


    /** List of all available host endpoints. */
    private List < HostEndpoint > _hostEndpoints;

    /** The map of active pools in the system. */
    private final Map < LegStarAddress, ConnectionPool > _pools;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Create a new pool manager.
     * @param hostEndpoints the list of all available host endpoints
     */
    public ConnectionPoolManager(
            final List < HostEndpoint > hostEndpoints) {
        _hostEndpoints = hostEndpoints;
        _pools = new HashMap < LegStarAddress, ConnectionPool >();
        _log.info("Pool Manager created.");
    }

    /**
     * Return a pool matching the request criteria.
     * @param address the host address to match
     * @param createIfNotFound if pool not found, one will be created
     * @return the pool matching the criteria
     * @throws ConnectionPoolException if pool cannot be created
     */
    public ConnectionPool getPool(
            final LegStarAddress address,
            final boolean createIfNotFound) throws ConnectionPoolException {
        if (_log.isDebugEnabled()) {
            _log.debug("Retrieving pool for endpoint " + address.getEndPointName());
        }
        ConnectionPool  pool = _pools.get(address);
        if (pool == null && createIfNotFound) {
            pool = createConnectionPool(address);
            _pools.put(address, pool);
        }
        return pool;
    }

    /**
     * Create a new connection pool for a given address. The address gives
     * an endpoint for which configuration data should exist.
     * @param address the pool common target host address
     * @return the new connection pool
     * @throws ConnectionPoolException in connection pool cannot be created
     */
    protected ConnectionPool createConnectionPool(
            final LegStarAddress address) throws ConnectionPoolException  {
        if (_log.isDebugEnabled()) {
            _log.debug("Creating new pool for endpoint " + address.getEndPointName());
        }
        HostEndpoint hostEndpoint = getHostEndpoint(address);
        ConnectionPool connectionPool = new ConnectionPool(address, hostEndpoint);
        return connectionPool;
    }

    /**
     * Retrieves a host endpoint for a given address. The address might be null
     * or with no endpoint name in which case we return the first endpoint as
     * the default.
     * @param address the target host address
     * @return an endpoint matching the address or the default one
     * @throws ConnectionPoolException if no match can be found
     */
    protected HostEndpoint getHostEndpoint(
            final LegStarAddress address) throws ConnectionPoolException {
        if (getHostEndpoints().size() == 0) {
            throw new ConnectionPoolException("No host endpoints available.");
        }
        if (address == null 
                || address.getEndPointName() == null
                || address.getEndPointName().length() == 0) {
            return getHostEndpoints().get(0);
        }
        for (HostEndpoint endpoint : getHostEndpoints()) {
            if (address.getEndPointName().equals(endpoint.getName())) {
                return endpoint;
            }
        }
        throw new ConnectionPoolException("No host endpoints matches " + address);
    }

    /**
     * Propagate shutdown on every managed pool.
     */
    public void shutDown() {
        _log.info("Shutting down Pool Manager");
        Iterator < Map.Entry < LegStarAddress, ConnectionPool > > entries =
            _pools.entrySet().iterator();
        while (entries.hasNext()) {
            entries.next().getValue().shutDown();
        }
    }

    /**
     * @return the active pools map
     */
    public Map < LegStarAddress, ConnectionPool > getPools() {
        return _pools;
    }

    /**
     * @return the list of available host endpoints
     */
    public List < HostEndpoint > getHostEndpoints() {
        return _hostEndpoints;
    }

}
