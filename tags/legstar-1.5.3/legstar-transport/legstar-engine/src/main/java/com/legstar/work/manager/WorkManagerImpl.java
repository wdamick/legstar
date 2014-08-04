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
package com.legstar.work.manager;

import java.util.Collection;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import commonj.work.Work;
import commonj.work.WorkEvent;
import commonj.work.WorkException;
import commonj.work.WorkItem;
import commonj.work.WorkListener;
import commonj.work.WorkManager;

/**
 * This Work Manager implementation can be used when there is none provided (by
 * a J2EE container for instance). Original work from Apache Software
 * Foundation.
 */
public class WorkManagerImpl implements WorkManager {

    /** Thread-pool. */
    private ExecutorService mExecutor;

    /** Logger. */
    private final Log _log = LogFactory.getLog(WorkManagerImpl.class);

    /**
     * Creates a work manager from an executor.
     * 
     * @param executor Thread-pool for work unit execution.
     */
    public WorkManagerImpl(final ExecutorService executor) {
        mExecutor = executor;
    }

    /**
     * Schedules a unit of work asynchronously.
     * 
     * @param work Work that needs to be scheduled.
     * @return Work Work item representing the asynchronous work
     */
    public WorkItem schedule(final Work work) {
        return schedule(work, null);
    }

    /**
     * Schedules a unit of work asynchronously.
     * 
     * @param work Work that needs to be scheduled.
     * @param workListener Work listener for callbacks.
     * @return Work Work item representing the asynchronous work
     */
    public WorkItem schedule(final Work work, final WorkListener workListener) {
        WorkItemImpl workItem = new WorkItemImpl(UUID.randomUUID().toString(),
                work);
        if (_log.isDebugEnabled()) {
            _log.debug("Scheduling new work item " + workItem.getId());
        }
        try {
            mExecutor.execute(new DecoratingWork(workItem, work, workListener));
            workAccepted(workItem, workListener);
        } catch (RejectedExecutionException e) {
            workItem.setException(new WorkException(e));
            workRejected(workItem, workListener);
        }
        return workItem;
    }

    /**
     * Wait for all the specified units of work to finish.
     * 
     * @param works Units of the work that need to finish.
     * @param timeout Timeout for waiting for the units of work to finish.
     * @return true if wait for all succeeded
     */
    @SuppressWarnings("rawtypes")
    public boolean waitForAll(final Collection works, final long timeout) {
        throw new UnsupportedOperationException("waitForAll not supported");
    }

    /**
     * Wait for any of the specified units of work to finish.
     * 
     * @param works Units of the work that need to finish.
     * @param timeout Timeout for waiting for the units of work to finish.
     * @return true if wait for any succeeded
     */
    @SuppressWarnings("rawtypes")
    public Collection waitForAny(final Collection works, final long timeout) {
        throw new UnsupportedOperationException("waitForAny not supported");
    }

    /**
     * Method provided for subclasses to indicate a work acceptance.
     * 
     * @param workItem Work item representing the work that was accepted.
     * @param workListener Work listener for callbacks.
     */
    private void workAccepted(final WorkItemImpl workItem,
            final WorkListener workListener) {
        synchronized (workItem) {
            if (_log.isDebugEnabled()) {
                _log.debug("Work item " + workItem.getId() + " accepted");
            }
            workItem.setStatus(WorkEvent.WORK_ACCEPTED);
            if (workListener != null) {
                WorkEvent event = new WorkEventImpl(workItem);
                workListener.workAccepted(event);
            }
        }
    }

    /**
     * Method to indicate a work was rejected.
     * 
     * @param workItem Work item representing the work that was started.
     * @param workListener Work listener for callbacks.
     */
    private void workRejected(final WorkItemImpl workItem,
            final WorkListener workListener) {
        synchronized (workItem) {
            if (_log.isDebugEnabled()) {
                _log.debug("Work item " + workItem.getId() + " rejected");
            }
            workItem.setStatus(WorkEvent.WORK_REJECTED);
            if (workListener != null) {
                workListener.workRejected(new WorkEventImpl(workItem));
            }
        }
    }

    /**
     * Method to indicate a work start.
     * 
     * @param workItem Work item representing the work that was started.
     * @param decoratingWork Work that was started with additional properties.
     */
    private void workStarted(final WorkItemImpl workItem,
            final DecoratingWork decoratingWork) {
        synchronized (workItem) {
            if (_log.isDebugEnabled()) {
                _log.debug("Work item " + workItem.getId() + " started");
            }
            workItem.setStatus(WorkEvent.WORK_STARTED);
            WorkListener workListener = decoratingWork.getWorkListener();
            if (workListener != null) {
                WorkEvent event = new WorkEventImpl(workItem);
                workListener.workStarted(event);
            }
        }
    }

    /**
     * Method to indicate a work completion.
     * 
     * @param workItem Work item representing the work that was completed.
     * @param decoratingWork Work that was completed with additional properties.
     */
    private void workCompleted(final WorkItemImpl workItem,
            final DecoratingWork decoratingWork) {
        workCompleted(workItem, decoratingWork, null);
    }

    /**
     * Method to indicate a work completion with an exception.
     * 
     * @param workItem Work item representing the work that was completed.
     * @param decoratingWork Work that was completed with additional properties.
     * @param exception the exception that was raised
     */
    private void workCompleted(final WorkItemImpl workItem,
            final DecoratingWork decoratingWork, final WorkException exception) {
        synchronized (workItem) {
            if (_log.isDebugEnabled()) {
                _log.debug("Work item " + workItem.getId() + " completed");
            }
            workItem.setStatus(WorkEvent.WORK_COMPLETED);
            workItem.setResult(decoratingWork.getDecoratedWork());
            workItem.setException(exception);
            if (_log.isDebugEnabled()) {
                _log.debug("Work item " + workItem.getId()
                        + " updated with results");
            }
            WorkListener workListener = decoratingWork.getWorkListener();
            if (workListener != null) {
                WorkEvent event = new WorkEventImpl(workItem);
                workListener.workCompleted(event);
            }
        }
    }

    /**
     * Class that decorates the original worker so that it can get callbacks
     * when work is done.
     */
    private final class DecoratingWork implements Runnable {

        /** Work item for this work. */
        private WorkItemImpl mWorkItem;

        /** The original work. */
        private Work mDecoratedWork;

        /** Observer listening on this work item events. */
        private WorkListener mWorkListener;

        /**
         * Initializes the work item and underlying work.
         * 
         * @param workItem Work item to be scheduled.
         * @param decoratedWork original work.
         * @param workListener an listener on the work events.
         */
        public DecoratingWork(final WorkItemImpl workItem,
                final Work decoratedWork, final WorkListener workListener) {
            mWorkItem = workItem;
            mDecoratedWork = decoratedWork;
            mWorkListener = workListener;
        }

        /**
         * Overrides the run method.
         */
        public void run() {
            workStarted(mWorkItem, this);
            try {
                mDecoratedWork.run();
                workCompleted(mWorkItem, this);
            } catch (Throwable th) {
                _log.error("Work item: " + mWorkItem.getId() + " failed", th);
                workCompleted(mWorkItem, this,
                        new WorkException(th.getMessage(), th));
            }
        }

        /**
         * @return the current work listener
         */
        public WorkListener getWorkListener() {
            return mWorkListener;
        }

        /**
         * @return the decorated work
         */
        public Work getDecoratedWork() {
            return mDecoratedWork;
        }
    }

}
