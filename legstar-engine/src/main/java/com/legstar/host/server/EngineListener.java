package com.legstar.host.server;

import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 
import commonj.work.WorkEvent;
import commonj.work.WorkListener;

/**
 * This class gets notifications from the work manager during the
 * engine lifecycle.
  */
public class EngineListener implements WorkListener {
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(EngineListener.class);
	
	/**
	 * The work manager has accepted the request.
	 * @param workEvent the work event
	 * */
	public final void workAccepted(final WorkEvent workEvent) {
		LOG.debug("Engine Work accepted.");
	}

	/**
	 * The Engine completed its work.
	 * @param workEvent the work event
	 * */
	public final void workCompleted(final WorkEvent workEvent) {
		if (workEvent.getException() != null) {
			LOG.fatal("Engine crashed", workEvent.getException());
		}
		LOG.debug("Engine stopped.");
	}

	/**
	 * Manager rejected that work.
	 * @param workEvent the work event
	 * */
	public final void workRejected(final WorkEvent workEvent) {
		LOG.debug("Engine work rejected.");
	}

	/**
	 * Work unit has started.
	 * @param workEvent the work event
	 *  */
	public final void workStarted(final WorkEvent workEvent) {
		LOG.debug("Engine started.");
	}
		
}
