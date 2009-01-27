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
package com.legstar.work.manager;

import commonj.work.WorkEvent;
import commonj.work.WorkException;
import commonj.work.WorkItem;

/**
 * Implements a <code>WorkEvent</code> as required by the commonj spec.
 * Original work from Apache Software Foundation.
 */
public class WorkEventImpl implements WorkEvent {

    /** Work item for this event. */
    private WorkItemImpl mWorkItem;

    /**
     * Instantiates the event.
     *
     * @param workItem Work item for this event.
     */
    public WorkEventImpl(final WorkItemImpl workItem) {
        mWorkItem = workItem;
    }

    /**
     * Returns the work type based on whether the work was accepted, started,
     * rejected or completed.
     *
     * @return Work type.
     */
    public final int getType() {
        return mWorkItem.getStatus();
    }

    /**
     * Returns the work item associated with this work type.
     *
     * @return Work item.
     */
    public final WorkItem getWorkItem() {
        return mWorkItem;
    }

    /**
     * Returns the exception if the work completed with an exception.
     *
     * @return Work exception.
     */
    public final WorkException getException() {
        return mWorkItem.getException();
    }
}
