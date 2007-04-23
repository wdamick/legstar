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
