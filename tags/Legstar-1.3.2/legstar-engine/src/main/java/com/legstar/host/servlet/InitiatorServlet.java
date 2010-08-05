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
package com.legstar.host.servlet;


import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 

import com.legstar.config.LegStarConfigurationException;
import com.legstar.config.commons.LegStarConfigCommons;
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

    /** Identifier for the adapter instance in the servlet context. */ 
    public static final String ENGINE_HANDLER_ID =
        "com.legstar.host.servlet.engineHandler";

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(InitiatorServlet.class);

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
    public void init(
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
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(configFileName);
            EngineHandler serverHandler = new EngineHandler(
                    legStarConfig.getPoolingEngineConfig());
            
            serverHandler.init();
            ServletContext servletContext = config.getServletContext();
            servletContext.setAttribute(ENGINE_HANDLER_ID, serverHandler);
            
        } catch (EngineStartupException e) {
            LOG.error("Failed to start engine.", e);
            throw new ServletException(e);
        } catch (LegStarConfigurationException e) {
            LOG.error("Failed to start engine.", e);
            throw new ServletException(e);
        }
    }

}
