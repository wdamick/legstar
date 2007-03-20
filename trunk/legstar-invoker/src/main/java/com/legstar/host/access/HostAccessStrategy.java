package com.legstar.host.access;

import com.legstar.messaging.Request;

/**
 * An abstract host access strategy providing capability to execute a request.
 */
public interface HostAccessStrategy {
	
	/**
	 * Synchronous execution for a given request.
	 * @param request the request details
	 * @throws HostAccessStrategyException if execution fails
	 */
	void invoke(Request request) throws HostAccessStrategyException;

}
