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
package com.legstar.host.server;

import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 

import com.legstar.config.PoolingEngineConfig;


/**
 * The role of the engine handler is to call the engine factory static methods
 * in order to create a singleton instance of an engine in the current VM.
 */
public class EngineHandler {

    /** Logger. */
    private final Log _log = LogFactory.getLog(EngineHandler.class);
    
    /** Pooling engine configuration bean.*/
    private PoolingEngineConfig _poolingEngineConfig;

    /**
     * Construct from an XML configuration.
     * @param poolingEngineConfig the configuration hierarchy
     */
    public EngineHandler(final PoolingEngineConfig poolingEngineConfig) {
        _poolingEngineConfig = poolingEngineConfig;
    }

    /**
     * Use holder to startup the engine.
     * @throws EngineStartupException if engine wouldn't start
     */
    public void init() throws EngineStartupException {
        _log.info("Initializing Engine.");
        try {
            EngineHolder.preInit(_poolingEngineConfig);
        } catch (EngineConfigurationException e) {
            throw new EngineStartupException(e);
        }
        EngineHolder.init();
    }

    /** Terminate the engine. */
    public void stop() {
        EngineHolder.stop();
    }

    /**
     * Returns a reference to the singleton engine.
     * @return the active engine
     * @throws EngineNotStartedException if engine is not active
     */
    public Engine getEngine() throws EngineNotStartedException {
        return EngineHolder.getEngine();
    }

}
