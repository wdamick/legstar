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
package com.legstar.host.server;

import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 

/**
 * The role of the engine handler is to call the engine factory static methods
 * in order to create a singleton instance of an engine in the current VM.
 */
public class EngineHandler {
	
	/** Engine configuration hierarchy. */
	private XMLConfiguration mConfig;
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(EngineHandler.class);
	
	/**
	 * Construct from an XML configuration.
	 * @param config the configuration hierarchy
	 */
	public EngineHandler(final XMLConfiguration config) {
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
