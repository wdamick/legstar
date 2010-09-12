package com.legstar.codegen.tasks;

import java.io.PrintStream;

import org.apache.commons.logging.Log;
import org.apache.tools.ant.BuildEvent;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.BuildLogger;
import org.apache.tools.ant.Project;

/**
 * This allows ant scripts to use commons-logging to produce logs.
 * This way there is a single log mechanism throughout the product..
 */
public class CommonsLoggingListener implements BuildListener, BuildLogger {

    /** The commons logging logger. */
    private Log _log;

    /**
     * Constructor using the caller logger.
     * 
     * @param log the caller logger
     */
    public CommonsLoggingListener(final Log log) {
        _log = log;
    }

    /**
     * @see BuildListener#buildStarted
     *      {@inheritDoc}
     */
    public void buildStarted(final BuildEvent event) {
        _log.info("Build started.");
    }

    /**
     * @see BuildListener#buildFinished
     *      {@inheritDoc}
     */
    public void buildFinished(final BuildEvent event) {

        if (event.getException() == null) {
            _log.info("Build finished.");
        } else {
            _log.error("Build finished with error.", event.getException());
        }
    }

    /**
     * @see BuildListener#targetStarted
     *      {@inheritDoc}
     */
    public void targetStarted(final BuildEvent event) {
        _log.debug("Start: " + event.getTarget().getName());
    }

    /**
     * @see BuildListener#targetFinished
     *      {@inheritDoc}
     */
    public void targetFinished(final BuildEvent event) {
        String targetName = event.getTarget().getName();
        if (event.getException() == null) {
            _log.debug("Target end: " + targetName);
        } else {
            _log.error("Target \"" + targetName
                    + "\" finished with error.", event.getException());
        }
    }

    /**
     * @see BuildListener#taskStarted
     *      {@inheritDoc}
     */
    public void taskStarted(final BuildEvent event) {
        if (_log.isDebugEnabled()) {
            _log.debug("Task \"" + event.getTask().getTaskName()
                    + "\" started.");
        }
    }

    /**
     * @see BuildListener#taskFinished
     *      {@inheritDoc}
     */
    public void taskFinished(final BuildEvent event) {
        if (event.getException() == null) {
            if (_log.isDebugEnabled()) {
                _log.debug("Task \"" + event.getTask().getTaskName()
                        + "\" finished.");
            }
        } else {
            _log.error("Target \"" + event.getTask().getTaskName()
                    + "\" finished with error.", event.getException());
        }
    }

    /**
     * @see BuildListener#messageLogged
     *      {@inheritDoc}
     */
    public void messageLogged(final BuildEvent event) {
        int priority = event.getPriority();
        String message = event.getMessage();
        switch (priority) {
        case Project.MSG_INFO:
            _log.info(message);
            break;
        case Project.MSG_WARN:
            _log.warn(message);
            break;
        case Project.MSG_ERR:
            _log.error(message);
            break;
        case Project.MSG_DEBUG:
            _log.debug(message);
            break;
        case Project.MSG_VERBOSE:
            _log.debug(message);
        default:
            break;
        }
    }

    /** {@inheritDoc} */
    public void setMessageOutputLevel(final int level) {
    }

    /** {@inheritDoc} */
    public void setOutputPrintStream(final PrintStream output) {
    }

    /** {@inheritDoc} */
    public void setEmacsMode(final boolean emacsMode) {
    }

    /** {@inheritDoc} */
    public void setErrorPrintStream(final PrintStream err) {
    }

}
