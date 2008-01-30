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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.host.access.HostAccessStrategy;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;


/**
 * An invoker has the capability to run a host program using java object trees
 * as input and output structures. The host program attributes are loaded from
 * a properties files and an XML configuration is expected to provide host
 * access parameters. The actual transport used is abstracted by dynamically
 * loading a host accessor based on a factory name from the XML configuration.
 */
public class CommareaInvoker extends CobolInvoker implements HostInvoker {
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CommareaInvoker.class);
	
	/** Direct or Pooled host access strategy. */
	private HostAccessStrategy mHostAccessStrategy;
	
	/** Host endpoint targeted. */
	private LegStarAddress mAddress;
	
	/** Host program attributes. */
	private CicsProgram mCicsProgram;
	
	/**
	 * Commarea Invoker calls a CICS Commarea-driven program. A commarea is a
	 * single input/output buffer parameter.
	 * @param hostAccessStrategy the pooled or direct access configured
	 * @param completeAddress the host address along with credentials
	 * @param hostProgram the host program attributes
	 * @throws HostInvokerException in construction fails
	 */
	public CommareaInvoker(
			final HostAccessStrategy hostAccessStrategy,
			final LegStarAddress completeAddress,
			final CicsProgram hostProgram) throws HostInvokerException {
		super(completeAddress.getHostCharset());
		mHostAccessStrategy = hostAccessStrategy;
		mAddress = completeAddress;
		mCicsProgram = hostProgram;
	}
	
	/**
	 * Invoke a commarea-driven program.
	 * @param requestID an identifier for this request (used for tracing)
	 * @param ccbin the input object tree
	 * @param ccbout the output object tree
	 * @throws HostInvokerException if invoke fails
	 */
	public final void invoke(
			final String requestID,
			final ICobolComplexBinding ccbin,
			final ICobolComplexBinding ccbout) throws HostInvokerException {
		
		long start = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
 			LOG.debug("Invoke Commarea started");
		}
		/* Allocate a host data buffer for the request */
		byte[] hostInputBytes = new byte[mCicsProgram.getLength()];
		
		/* Convert from java to host */
		int dataLength = marshal(ccbin, hostInputBytes);
		
		/* Adjust the program data length if needed */
		if (dataLength < mCicsProgram.getDataLength()) {
			mCicsProgram.setDataLength(dataLength);
		}
		
		/* Create a request instance and call the host program */
		LegStarRequest request = createCommareaRequest(requestID, mAddress,
				hostInputBytes);
		try {
			mHostAccessStrategy.invoke(request);
		} catch (HostAccessStrategyException e) {
			throw new HostInvokerException(e);
		}
		
		/* The request might have failed */
		if (request.getException() != null) {
			throw new HostInvokerException(request.getException());
		}
		/* Since this is a commarea driven program, a single data
		 * part is expected back  */
		if (request.getResponseMessage().getDataParts().size() > 0) {
			unmarshal(request.getResponseMessage().getDataParts().
					get(0).getContent(), ccbout);
		}

		if (LOG.isDebugEnabled()) {
			long end = System.currentTimeMillis();
			LOG.debug("Invoke Commarea ended. elapse: "
					+ Long.toString(end - start) + " ms");
		}
	}
	
	/**
	 * This method is invalid for Commarea-driven programs. In a future
	 * version it will be supported to allow for a commarea to be mapped 
	 * to multiple Cobol structures.
	 * @param requestID an identifier for this request (used for tracing)
	 * @param inParts a set of input object trees with target containers
	 * @param outParts a set of output object trees with source containers
	 * @throws HostInvokerException if invoke fails
	 */
	public final void invoke(
			final String requestID,
			final Map < String, ICobolComplexBinding > inParts,
			final Map < String, ICobolComplexBinding > outParts)
			throws HostInvokerException {
		throw new HostInvokerException(
			"Unsupported method for CICS commarea");
	}

	/**
	 * Creates a request for a commarea-driven program.
	 * @param requestID a request traceability identifier
	 * @param address the host endpoint
	 * @param hostInputBytes the input binary content
	 * @return the request ready to be submitted
	 * @throws HostInvokerException if failed to create request
	 */
	private LegStarRequest createCommareaRequest(
			final String requestID,
			final LegStarAddress address,
			final byte[] hostInputBytes) throws HostInvokerException {
		
		List < LegStarMessagePart > dataParts =
			new ArrayList < LegStarMessagePart >();
		dataParts.add(new CommareaPart(hostInputBytes));
		LegStarHeaderPart headerPart;
		try {
			headerPart = new LegStarHeaderPart(
				mCicsProgram.getProgramAttrMap(), dataParts.size());
		} catch (HeaderPartException e) {
			throw new HostInvokerException(e);
		}
		LegStarMessage requestMessage =
			new LegStarMessage(headerPart, dataParts);
		return new LegStarRequest(requestID, address, requestMessage);
	}

	/**
	 * @return the hpst address
	 */
	public final LegStarAddress getAddress() {
		return mAddress;
	}

	/**
	 * @return the host access strategy
	 */
	public final HostAccessStrategy getHostAccessStrategy() {
		return mHostAccessStrategy;
	}

	/**
	 * @return the programme attributes
	 */
	public final CicsProgram getProgramAttr() {
		return mCicsProgram;
	}

}
