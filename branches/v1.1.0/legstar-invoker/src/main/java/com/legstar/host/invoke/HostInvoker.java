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
package com.legstar.host.invoke;

import java.util.Map;

import com.legstar.coxb.ICobolComplexBinding;

/**
 * Abstract class to invoke host functions.
 */
public interface HostInvoker {
	
	/**
	 * Invoke a host function using one input and one output structure.
	 * @param requestID an identifier for this request (used for tracing)
	 * @param ccbin the input object tree
	 * @param ccbout the output object tree
	 * @throws HostInvokerException if invoke fails
	 */
	void invoke(
			String requestID,
			ICobolComplexBinding ccbin,
			ICobolComplexBinding ccbout) throws HostInvokerException;
	
	/**
	 * Invoke a host function with multiple input and multiple output parts.
	 * @param requestID an identifier for this request (used for tracing)
	 * @param inParts a set of input object trees
	 * @param outParts a set of output object trees
	 * @throws HostInvokerException if invoke fails
	 */
	void invoke(
			String requestID,
			Map < String, ICobolComplexBinding > inParts,
			Map < String, ICobolComplexBinding > outParts)
			throws HostInvokerException;

}
