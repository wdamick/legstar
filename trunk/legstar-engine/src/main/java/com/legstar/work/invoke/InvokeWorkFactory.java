package com.legstar.work.invoke;

import com.legstar.host.server.WorkFactory;
import com.legstar.messaging.Connection;
import com.legstar.messaging.Request;
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
			final Request request, final Connection connection) {
		return new InvokeWork(request, connection);
	}

	/** (non-Javadoc).
	 * @see com.legstar.host.server.WorkFactory#createWorkListener(
	 * com.legstar.messaging.Request, com.legstar.messaging.Connection,
	 *  com.legstar.pool.manager.ConnectionPoolInterface)
	 * {@inheritDoc}
	 */
	public final WorkListener createWorkListener(
			final Request request,
			final Connection connection,
			final ConnectionPool pool) {
		return new InvokeWorkListener(request, connection, pool);
	}

}
