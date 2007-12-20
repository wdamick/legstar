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

import commonj.work.Work;
import commonj.work.WorkException;
import commonj.work.WorkItem;

/**
 * Implements a <code>WorkItem</code> as required by the commonj spec.
  * Original work from Apache Software Foundation.
*/
class LsWorkItem implements WorkItem {

	/** Identifier if this work item. */
	private String mId;

	/** Completion status. */
	private int mStatus = -1;

	/** the completed Work or null if the Work is not yet complete. */
	private Work mResult;

	/** The work unit from which this item originates. */
	private Work mOriginalWork;

	/** If work unit failed, Exception otherwise null.  */
	private WorkException mException;

	/**
	 * Creates a work item.
	 *
	 * @param id of this work event.
	 * @param orginalWork associated work unit
	 */
	protected LsWorkItem(final String id, final Work orginalWork) {
		mId = id;
		mOriginalWork = orginalWork;
		mResult = null;
		mException = null;
	}
	
	/**
	 * Returns the id.
	 *
	 * @return Id of this item.
	 */
	public String getId() {
	    return mId;
	}

	/**
	 * Returns the original work.
	 *
	 * @return Original work.
	 */
	public Work getOriginalWork() {
	    return mOriginalWork;
	}

	/**
	 * Returns the work result if the work completed.
	 *
	 * @return Work.
	 * @throws WorkException If the work completed with an exception.
	 */
	public Work getResult() throws WorkException {
		if (mException != null) {
			throw mException;
		}
	    return mResult;
	}

	/**
	 * Sets the result.
	 *
	 * @param result Result.
	 */
	protected void setResult(final Work result) {
	    mResult = result;
	}

	/**
	 * Returns the exception if work completed with an exception.
	 *
	 * @return Work exception.
	 */
	protected WorkException getException() {
	    return mException;
	}
	/**
	 * Sets the exception.
	 *
	 * @param exception Exception.
	 */
	protected void setException(final WorkException exception) {
		mException = exception;
	}
	
	/**
	 * Returns the work status (whether the work was accepted, started,
	 * rejected or completed).
	 *
	 * @return Work status.
	 */
	public int getStatus() {
	    return mStatus;
	}
	
	/**
	 * Sets the status.
	 *
	 * @param status Status.
	 */
	protected void setStatus(final int status) {
	    mStatus = status;
	}
	/**
	 * @see Object#hashCode() 
	 * {@inheritDoc}
	 */
	public final int hashCode() {
	    return mId.hashCode();
	}
	/**
	 * Indicates whether some other object is "equal to" this one.
	 *
	 * @param obj Object to be compared.
	 * @return true if this object is the same as the obj argument; false
	 *         otherwise..
	 */
	public boolean equals(final Object obj) {
	    return (obj != null) && (obj.getClass() == LsWorkItem.class)
	    	&& ((LsWorkItem) obj).mId.equals(mId);
	}
	
	/**
	 * Compares this object with the specified object for order. Returns a
	 * negative integer, zero, or a positive integer as this object is less
	 * than, equal to, or greater than the specified object.
	 *
	 * @param o Object to be compared.
	 * @return A negative integer, zero, or a positive integer as this object
	 *         is less than, equal to, or greater than the specified object.
	 */
	public int compareTo(final Object o) {
	    if (o.getClass() != LsWorkItem.class) {
	        throw new ClassCastException(o.getClass().getName());
	    } else {
	        return ((LsWorkItem) o).getId().compareTo(getId());
	    }
	}
}
