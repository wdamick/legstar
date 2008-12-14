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
package com.legstar.work.invoke;

import com.legstar.host.server.WorkFactory;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarRequest;
import com.legstar.pool.manager.ConnectionPool;
import commonj.work.Work;
import commonj.work.WorkListener;

/**
 * Concrete factory to create units of work and listeners.
 */
public class InvokeWorkFactory implements WorkFactory {

    /** (non-Javadoc).
     * @see com.legstar.host.server.WorkFactory#createWork(
     * com.legstar.messaging.Request, com.legstar.messaging.Connection)
     * {@inheritDoc}
     */
    public final Work createWork(
            final LegStarRequest request, final LegStarConnection connection) {
        return new InvokeWork(request, connection);
    }

    /** (non-Javadoc).
     * @see com.legstar.host.server.WorkFactory#createWorkListener(
     * com.legstar.messaging.Request, com.legstar.messaging.Connection,
     *  com.legstar.pool.manager.ConnectionPoolInterface)
     * {@inheritDoc}
     */
    public final WorkListener createWorkListener(
            final LegStarRequest request,
            final LegStarConnection connection,
            final ConnectionPool pool) {
        return new InvokeWorkListener(request, connection, pool);
    }

}
