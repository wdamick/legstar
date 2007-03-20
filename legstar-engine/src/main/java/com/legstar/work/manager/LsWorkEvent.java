package com.legstar.work.manager;

import commonj.work.WorkEvent;
import commonj.work.WorkException;
import commonj.work.WorkItem;

/**
 * Implements a <code>WorkEvent</code> as required by the commonj spec.
 * Original work from Apache Software Foundation.
 */
public class LsWorkEvent implements WorkEvent {

	/** Work item for this event. */
	private WorkItem mWorkItem;

	/** Exception if something has gone wrong.  */
	private WorkException mException;
	
	/**
	 * Instantiates the event.
	 *
	 * @param workItem Work item for this event.
	 */
	public LsWorkEvent(final LsWorkItem workItem) {
		mWorkItem = workItem;
		mException = workItem.getException();
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
	    return mException;
	}
}
