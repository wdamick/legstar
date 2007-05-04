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
import java.rmi.server.UID;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;

import commonj.work.Work;
import commonj.work.WorkEvent;
import commonj.work.WorkException;
import commonj.work.WorkItem;
import commonj.work.WorkListener;
import commonj.work.WorkManager;
import commonj.work.WorkRejectedException;

/**
 * This Work Manager implementation can be used when there is none provided
 * (by a J2EE container for instance).
 * Original work from Apache Software Foundation.
 */
public class LsWorkManager implements WorkManager {
	
	/** Map of work items currently handled by the work manager. */
	private Map < LsWorkItem, WorkListener > mWorkItems =
		new ConcurrentHashMap < LsWorkItem, WorkListener >();

	/** Thread-pool. */
	private ExecutorService mExecutor;
	
	/**
	 * Creates a work manager with a fixed size thread pool.
	 * 
	 * @param executor Thread-pool manager.
	 */
	public LsWorkManager(final ExecutorService executor) {
		mExecutor = executor;
	}
	/**
	 * Schedules a unit of work asynchronously.
	 * 
	 * @param work Work that needs to be scheduled.
	 * @return Work Work item representing the asynchronous work
	 * @throws WorkException if scheduling fails
	 */
	public final WorkItem schedule(final Work work) throws WorkException {
	    return schedule(work, null);
	}
	/**
	 * Schedules a unit of work asynchronously.
	 * 
	 * @param work Work that needs to be scheduled.
	 * @param workListener Work listener for callbacks.
	 * @return Work Work item representing the asynchronous work
	 * @throws WorkRejectedException if work is rejected
	 */
	public final WorkItem schedule(
			final Work work,
			final WorkListener workListener) throws WorkRejectedException {
 	    LsWorkItem workItem = new LsWorkItem(new UID().toString(), work);
	    if (scheduleWork(workItem, work)) {
	        if (workListener != null) {
	        	mWorkItems.put(workItem, workListener);
	        }
	        workAccepted(workItem, work);
	        return workItem;
	    } else {
	        workItem.setStatus(WorkEvent.WORK_REJECTED);
	        if (workListener != null) {
	            workListener.workRejected(new LsWorkEvent(workItem));
	        }
	        throw new WorkRejectedException("Unable to schedule work");
	    }
	}
	
	/**
	 * Wait for all the specified units of work to finish.
	 * 
	 * @param works Units of the work that need to finish.
	 * @param timeout Timeout for waiting for the units of work to finish.
	 * @return true if wait for all succeeded
	 */
	public final boolean waitForAll(
			final Collection works, final long timeout) {
	    throw new UnsupportedOperationException("waitForAll not supported");
	}
	
	/**
	 * Wait for any of the specified units of work to finish.
	 * 
	 * @param works Units of the work that need to finish.
	 * @param timeout Timeout for waiting for the units of work to finish.
	 * @return true if wait for any succeeded
	 */
	public final Collection waitForAny(
			final Collection works, final long timeout) {
	    throw new UnsupportedOperationException("waitForAny not supported");
	}
	
	/**
	 * Method provided for subclasses to indicate a work accptance.
	 *
	 * @param workItem Work item representing the work that was accepted.
	 * @param work     Work that was accepted.
	 */
	private void workAccepted(
			final LsWorkItem workItem, final Work work) {
 	    WorkListener listener = (WorkListener) mWorkItems.get(workItem);
	    if (listener != null) {
	        workItem.setStatus(WorkEvent.WORK_ACCEPTED);
	        WorkEvent event = new LsWorkEvent(workItem);
	        listener.workAccepted(event);
	    }
	}
	
	/**
	 * Method to indicate a work start.
	 * @param workItem Work item representing the work that was started.
	 * @param work     Work that was started.
	 */
	private synchronized void workStarted(
			final LsWorkItem workItem, final Work work) {
	    WorkListener listener = (WorkListener) mWorkItems.get(workItem);
	    if (listener != null) {
	        workItem.setStatus(WorkEvent.WORK_STARTED);
	        WorkEvent event = new LsWorkEvent(workItem);
	        listener.workStarted(event);
	    }
	}
	
	/**
	 * Method to indicate a work completion.
	 * @param workItem Work item representing the work that was completed.
	 * @param work     Work that was completed.
	 */
	private void workCompleted(
			final LsWorkItem workItem, final Work work) {
	    workCompleted(workItem, work, null);
	}
	
	/**
	 * Method to indicate a work completion with an exception.
	 * @param workItem Work item representing the work that was completed.
	 * @param work     Work that was completed.
	 * @param exception the exception that was raised
	 */
	private void workCompleted(
			final LsWorkItem workItem,
			final Work work,
			final WorkException exception) {
	    WorkListener listener = (WorkListener) mWorkItems.get(workItem);
	    if (listener != null) {
	        workItem.setStatus(WorkEvent.WORK_COMPLETED);
	        workItem.setResult(work);
	        workItem.setException(exception);
	        WorkEvent event = new LsWorkEvent(workItem);
	        listener.workCompleted(event);
	        mWorkItems.remove(workItem);
	    }
	}
	
	/**
	 * Schedules the work using the threadpool.
	 * @param workItem Work item representing the work that was completed.
	 * @param work     Work that was completed.
	 * @return true if scheduling was accepted
	 */
	private boolean scheduleWork(final LsWorkItem workItem, final Work work) {
	    try {
	        mExecutor.execute(new DecoratingWork(workItem, work));
	        return true;
	    } catch (RejectedExecutionException ex) {
	        return false;
	    }
	}
	
	/**
	 * Class that decorates the original worker so that it can get callbacks
	 * when work is done.
	 */
	private final class DecoratingWork implements Runnable {
		
	    /** Work item for this work. */
	    private LsWorkItem mWorkItem;
	    
	    /** The original work. */
	    private Work mDecoratedWork;
	    
	    /**
	     * Initializes the work item and underlying work.
	     * @param workItem Work item to be scheduled.
	     * @param decoratedWork original work.
	     */
	    private DecoratingWork(
	    		final LsWorkItem workItem,
	    		final Work decoratedWork) {
	    	mWorkItem = workItem;
	    	mDecoratedWork = decoratedWork;
	    }
	    
	    /**
	     * Overrides the run method.
	     */
	    public void run() {
	        workStarted(mWorkItem, mDecoratedWork);
	        try {
	        	mDecoratedWork.run();
	            workCompleted(mWorkItem, mDecoratedWork);
	        } catch (Throwable th) {
	            workCompleted(mWorkItem, mDecoratedWork,
	            		new WorkException(th.getMessage(), th));
	        }
	    }
	}

}
