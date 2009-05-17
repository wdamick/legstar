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
package com.legstar.mq.client;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.Config;
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.client.AbstractConnectionTester;

/**
 * Generic test helper class.
 *
 */
public abstract class AbstractMQConnectionTester extends AbstractConnectionTester {

    /** Configuration file.*/
    public static final String CONFIG_FILE = "config.xml";
    
    /** An endpoint. */
    private CicsMQEndpoint mEndpoint;
    
    /** Address of target host. */
    private LegStarAddress mAddress;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());
    /**
     * @return the Address of target host
     */
    public LegStarAddress getAddress() {
        return mAddress;
    }

    /**
     * @return the Http endpoint
     */
    public CicsMQEndpoint getEndpoint() {
        return mEndpoint;
    }

    /**
     * Special setup using an endpoint configuration name.
     * @param endpointName endpoint name
     * @throws Exception if setup fails
     */
    public void setUp(final String endpointName) throws Exception {
        HierarchicalConfiguration endpointConfig =
            Config.loadEndpointConfiguration(CONFIG_FILE, endpointName);
        mEndpoint = new CicsMQEndpoint(endpointConfig);
        if (_log.isDebugEnabled()) {
            mEndpoint.setHostTraceMode(true);
        }

        mAddress = new LegStarAddress(endpointConfig);
    }

}
