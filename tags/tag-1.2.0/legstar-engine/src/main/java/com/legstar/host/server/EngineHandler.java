/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.host.server;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 

/**
 * The role of the engine handler is to call the engine factory static methods
 * in order to create a singleton instance of an engine in the current VM.
 */
public class EngineHandler {
	
	/** Engine configuration hierarchy. */
	private HierarchicalConfiguration mConfig;
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(EngineHandler.class);
	
	/**
	 * Construct from an XML configuration.
	 * @param config the configuration hierarchy
	 */
	public EngineHandler(final HierarchicalConfiguration config) {
		mConfig = config;
	}
	
	/**
	 * Use holder to startup the engine.
	 * @throws EngineStartupException if engine wouldn't start
	 */
	public final void init() throws EngineStartupException {
		LOG.info("Initializing Engine.");
		try {
			EngineHolder.preInit(mConfig);
		} catch (EngineConfigurationException e) {
			throw new EngineStartupException(e);
		}
		EngineHolder.init();
	}
	
	/** Terminate the engine. */
	public final void stop() {
		EngineHolder.stop();
	}
	
	/**
	 * Returns a reference to the singleton engine.
	 * @return the active engine
	 * @throws EngineNotStartedException if engine is not active
	 */
	public final Engine getEngine() throws EngineNotStartedException {
		return EngineHolder.getEngine();
	}

}
