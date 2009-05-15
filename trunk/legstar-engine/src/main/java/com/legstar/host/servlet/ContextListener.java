package com.legstar.host.servlet;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.host.server.EngineHandler;

/**
 * We need to listen on the application shutdown in order to stop our engine properly.
 * The web.xml must declare this as a listener.
 *
 */
public final class ContextListener implements ServletContextListener  {

    /** Logger. */
    private final Log _log = LogFactory.getLog(ContextListener.class);

    /** {@inheritDoc}*/
    public void contextDestroyed(final ServletContextEvent event) {
        ServletContext context = event.getServletContext();
        EngineHandler engineHandler = (EngineHandler) context.getAttribute(
                InitiatorServlet.ENGINE_HANDLER_ID);
        engineHandler.stop();
        _log.info("LegStar engine context destroyed");
    }

    /** {@inheritDoc}*/
    public void contextInitialized(final ServletContextEvent arg0) {
        _log.info("LegStar engine context initialized");
    }
    
}

