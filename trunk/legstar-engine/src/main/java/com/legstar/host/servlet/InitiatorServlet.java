package com.legstar.host.servlet;


import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 

import com.legstar.host.server.EngineHandler;
import com.legstar.host.server.EngineStartupException;

/**
 * This Servlet is meant to run once, at startup type, and is in charge of
 * getting configuration data and hand over control to a handler.
  */
public class InitiatorServlet extends HttpServlet {

	/** Serial ID. */
	private static final long serialVersionUID = -4200681992731561770L;

	/** Config file name init param key. */
	public static final String CONFIG_PARAM = "engine.config";

	/** Engine handler implementation. */   
	private EngineHandler mServerHandler;

	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(InitiatorServlet.class);
	
	/**
	 * Servlet constructor.
	 */
	public InitiatorServlet() {
		super();
	}

	/**
	 * Initialization of the servlet. Loads configuration file and creates an
	 * instance of the engine handler.
	 *
	 * @param config the complete configuration hierarchy
	 * @throws ServletException if an error occurs
	 */
	public final void init(
			final ServletConfig config) throws ServletException {
		String configFileName = config.getInitParameter(CONFIG_PARAM);
		if (configFileName == null || configFileName.length() == 0) {
			throw new ServletException(
					"Web.xml does not contain the " + CONFIG_PARAM
					+ " parameter.");
		}
		
		LOG.info("Initializing with " + configFileName
				+ " configuration file.");
		
		try {
			mServerHandler = new EngineHandler(loadConfigFile(configFileName));
			mServerHandler.init();
		} catch (ConfigurationException e) {
			LOG.error("Failed to initialize.", e);
			throw new ServletException(e);
		} catch (EngineStartupException e) {
			LOG.error("Failed to start engine.", e);
			throw new ServletException(e);
		}
	}

	/**
	 * Destruction of the servlet.
	 */
	public final void destroy() {
		super.destroy(); 
		mServerHandler.stop();
		mServerHandler = null;
		LOG.info("Servlet destroyed");
	}


	/**
	 * Use the Apache configuration API to retrieve the configuration file.
	 * This gives q lot of flexibility to locate the file.
	 * 
	 * @param configFileName name of the configuration file
	 * @return the configuration retrieved
	 * @throws ConfigurationException if configuration cannot be retrieved
	 */
	private XMLConfiguration loadConfigFile(
			final String configFileName) throws ConfigurationException	{
		LOG.debug("Attempting to load " + configFileName);
		XMLConfiguration config = new XMLConfiguration(configFileName);
		LOG.debug("Load success for " + configFileName);
		return config; 
	}

}
