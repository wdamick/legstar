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
