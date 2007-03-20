package com.legstar.host.server;

import com.legstar.messaging.Connection;
import com.legstar.messaging.Request;
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
	Work createWork(Request request, Connection connection);
	
	
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
			Request request, Connection connection, ConnectionPool pool);

}
