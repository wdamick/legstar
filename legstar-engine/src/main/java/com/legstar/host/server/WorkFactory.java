/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.host.server;

import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarRequest;
import com.legstar.pool.manager.ConnectionPool;

import commonj.work.Work;
import commonj.work.WorkListener;

/**
 * Abstract factory shields engine from type of host connection used.
 *
 */
public interface WorkFactory {

    /**
     * Create a new unit of work.
     * @param request the request being serviced
     * @param connection the host connection to use
     * @return a Work element that can be submitted to a work manager
     */
    Work createWork(LegStarRequest request, LegStarConnection connection);


    /**
     * Create a new work unit listener. Such a listener gets notification from
     * the work manager when the job completed.
     * 
     * @param request the request being serviced
     * @param connection the connection used
     * @param pool the connection pool into which to recycle the connection
     *  when work is done
     * @return a work listener
     */
    WorkListener createWorkListener(
            LegStarRequest request, LegStarConnection connection,
            ConnectionPool pool);

}
