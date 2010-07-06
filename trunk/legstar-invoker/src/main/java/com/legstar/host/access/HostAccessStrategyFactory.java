/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.host.access;

import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.HostEndpoint.AccessStrategy;

/**
 * A factory providing a host access strategy. The host access strategy is
 * configurable.
 */
public class HostAccessStrategyFactory {

    /**
     * This factory is a utility class.
     */
    private HostAccessStrategyFactory() {

    }

    /**
     * Delivers a host access strategy.
     * @param endpoint target host endpoint
     * @return the new host access strategy
     * @throws HostAccessStrategyException if creation failed
     */
    public static HostAccessStrategy createAccessStrategy(
            final HostEndpoint endpoint)
    throws HostAccessStrategyException {
        if (endpoint.getHostAccessStrategy() == AccessStrategy.direct) {
            return new DirectHostAccessStrategy(endpoint);
        } else {
            if (endpoint.getHostAccessStrategy() == AccessStrategy.pooled) {
                return new PooledHostAccessStrategy(endpoint);
            } else {
                throw new HostAccessStrategyException(
                "Unknown host access strategy.");
            }
        }

    }

}
