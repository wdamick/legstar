package com.legstar.host.invoke;

import com.legstar.coxb.ICobolComplexBinding;

/**
 * Abstract class to invoke host functions.
 */
public interface HostInvoke {
	
	/**
	 * Invoke a host function using one input and one output structure.
	 * @param requestID an identifier for this request (used for tracing)
	 * @param ccbin the input object tree
	 * @param ccbout the output object tree
	 * @throws HostInvokeException if invoke fails
	 */
	void invoke(
			final String requestID,
			final ICobolComplexBinding ccbin,
			final ICobolComplexBinding ccbout) throws HostInvokeException;
	

}
