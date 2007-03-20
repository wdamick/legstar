package com.legstar.host.server;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.naming.InitialContext;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.pool.manager.ConnectionPoolManager;
import com.legstar.work.invoke.InvokeWorkFactory;
import com.legstar.work.manager.LsWorkManager;

import commonj.work.WorkException;
import commonj.work.WorkManager;

/**
 * This class holds a reference to a singleton Engine. Any client who needs
 * access to the engine will do so thru the engine holder.
 */
public final class EngineHolder {
	
	/** The singleton engine instance. */
	private static Engine sEngine;
	
	/** A thread pool to be used with default work manager implementation. */
	private static ExecutorService sExecutor;
	
	/** WorkManager implementation for asynchronous work. */
	private static WorkManager sWorkManager;
	
	/** Host connections pool manager. */
	private static ConnectionPoolManager sPoolManager;

	/** The current configuration settings. */
	private static XMLConfiguration sConfig;
	
	/** Configuration key giving the work manager JNDI location. */
	private static final String WM_LOCATION =
		"engine/workManager/threadPool/JNDILocation";
	
	/** Configuration key giving the work manager thread pool size. */
	private static final String WM_TPOOL_SIZE =
		"engine/workManager/defaultThreadPool/size";
	
	/** Configuration key giving maximum number of requests waiting to
	 *  be serviced. */
	private static final String ENGINE_MAX_REQUESTS = "engine/maxRequests";
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(EngineHolder.class);
	
	/** An Engine Holder cannot be instanciated. */
	private EngineHolder() {
	}

	/**
	 * Create the engine environment described in a configuration.
	 * @param config the complete configuration hierarchy
	 * @throws EngineConfigurationException if configuration is invalid
	 */
	public static void preInit(final XMLConfiguration config)
			throws EngineConfigurationException {
		sConfig = config;
		sConfig.setExpressionEngine(new XPathExpressionEngine());
		sPoolManager = new ConnectionPoolManager(sConfig);
		initializeWorkManager();
	}
	
	/**
	 * Create the single instance of an engine. 
	 * @throws EngineStartupException if engine fails to start
	 *  */
	public static void init() throws EngineStartupException {
		LOG.debug("Starting engine.");
		int maxRequests = sConfig.getInt(ENGINE_MAX_REQUESTS);
		sEngine = new Engine(maxRequests, sWorkManager, sPoolManager,
				new InvokeWorkFactory());
		try {
			sWorkManager.schedule(sEngine, new EngineListener());
		} catch (IllegalArgumentException e) {
			throw new EngineStartupException(e);
		} catch (WorkException e) {
			throw new EngineStartupException(e);
		}
	}
	
	/** Shutdown the engine. */
	public static void stop() {
		if (sEngine != null) {
			sEngine.shutDown();
			sEngine = null;
		}
		if (sPoolManager != null) {
			sPoolManager.shutDown();
			sPoolManager = null;
		}
		if (sExecutor != null) {
			sExecutor.shutdownNow();
			sExecutor = null;
		}
		sWorkManager = null;
	}
	
	/**
	 * @return the Engine singleton.
	 * @throws EngineNotStartedException if engine is unavailable
	 */
	public static Engine getEngine() throws EngineNotStartedException {
		if (sEngine == null) {
			throw new EngineNotStartedException(
					"The host access engine is not running.");
		}
		if (sEngine.isShuttingDown()) {
			throw new EngineNotStartedException(
					"The host access engine is shutting down.");
		}
		return sEngine;
	}

	/**
	* This method initializes the work manager used by the engine. We will
	* first attempt to lookup the work manager from the JNDI location
	* specified in the engine config file. If not specified, or unable to load,
	* we will use the default work manager.
	*/
	private static void initializeWorkManager() {
		LOG.debug("Initializing Work Manager.");
		String workMgrLocation = sConfig.getString(WM_LOCATION);
 		if (workMgrLocation != null && workMgrLocation.length() > 0) {
	        try  {
	        	InitialContext ic = new InitialContext();
	        	sWorkManager = (WorkManager) ic.lookup(workMgrLocation);
	        } catch (Exception e) {
	        	sWorkManager = null;
	        }
		} else {
			sWorkManager = null;
		}
  
		if (sWorkManager == null) {
			int threadPoolSize = sConfig.getInt(WM_TPOOL_SIZE);
			sExecutor = Executors.newFixedThreadPool(threadPoolSize);
			sWorkManager = new LsWorkManager(sExecutor);
		}
	}

}
