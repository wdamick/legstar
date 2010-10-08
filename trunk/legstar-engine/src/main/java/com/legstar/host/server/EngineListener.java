/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
    private final Log _log = LogFactory.getLog(EngineListener.class);

    /**
     * The work manager has accepted the request.
     * @param workEvent the work event
     * */
    public void workAccepted(final WorkEvent workEvent) {
        _log.debug("Engine Work accepted.");
    }

    /**
     * The Engine completed its work.
     * @param workEvent the work event
     * */
    public void workCompleted(final WorkEvent workEvent) {
        if (workEvent.getException() != null) {
            _log.fatal("Engine crashed", workEvent.getException());
        }
        _log.debug("Engine stopped.");
    }

    /**
     * Manager rejected that work.
     * @param workEvent the work event
     * */
    public void workRejected(final WorkEvent workEvent) {
        _log.debug("Engine work rejected.");
    }

    /**
     * Work unit has started.
     * @param workEvent the work event
     *  */
    public void workStarted(final WorkEvent workEvent) {
        _log.debug("Engine started.");
    }

}
