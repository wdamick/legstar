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
package com.legstar.host.access;


import com.legstar.host.AbstractTester;
import com.legstar.messaging.HostEndpoint;

/**
 * Test the HostAccessStrategyFactory.
 *
 */
public class HostAccessStrategyFactoryTest extends AbstractTester {

    /**
     * Test the default behavior.
     */
    public void testConstructorWithDefaultStrategy() {
        try {
            HostEndpoint endpoint = getStandardHostEndpoint();
            HostAccessStrategy has = HostAccessStrategyFactory.createAccessStrategy(endpoint);
            assertEquals("class com.legstar.host.access.DirectHostAccessStrategy",
                    has.getClass().toString());
        } catch (HostAccessStrategyException e) {
            fail("testConstructor failed " + e.getMessage());
        }
    }

    /**
     * Get a pooled configuration.
     */
    public void testConstructorWithPooledStrategy() {
        try {
            HostEndpoint endpoint = getPooledHostEndpoint();
            HostAccessStrategy has = HostAccessStrategyFactory.createAccessStrategy(endpoint);
            assertEquals("class com.legstar.host.access.PooledHostAccessStrategy", has.getClass().toString());
        } catch (HostAccessStrategyException e) {
            fail("testConstructor failed " + e.getMessage());
        }
    }

}
