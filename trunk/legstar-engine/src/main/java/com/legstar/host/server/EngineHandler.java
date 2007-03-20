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
