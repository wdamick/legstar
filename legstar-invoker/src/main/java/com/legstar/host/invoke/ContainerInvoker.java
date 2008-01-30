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
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;

/**
 * A ContainerInvoker builds a request object for a target CHANNEL-driven CICS
 * program. The request messages will be multi-part where each part corresponds
 * to a CICS CONTAINER.
 *
 */
public class ContainerInvoker extends CobolInvoker implements HostInvoker {

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(ContainerInvoker.class);

	/** Direct or Pooled host access strategy. */
	private HostAccessStrategy mHostAccessStrategy;
	
	/** Host endpoint targeted. */
	private LegStarAddress mAddress;
	
	/** Host program attributes. */
	private CicsProgram mCicsProgram;
	
	/**
	 * Container Invoker calls a CICS Container-driven program. Each container
	 * corresponds to one part in a multi-part input/output.
	 * @param hostAccessStrategy the pooled or direct access configured
	 * @param completeAddress the host address along with credentials
	 * @param hostProgram the host program attributes
	 * @throws HostInvokerException in construction fails
	 */
	public ContainerInvoker(
			final HostAccessStrategy hostAccessStrategy,
			final LegStarAddress completeAddress,
			final CicsProgram hostProgram) throws HostInvokerException {
		super(completeAddress.getHostCharset());
		mHostAccessStrategy = hostAccessStrategy;
		mAddress = completeAddress;
		mCicsProgram = hostProgram;
	}
	
	/**
	 * This method is invalid for Containers because the signature does not
	 * provide container names which are mandatory.
	 * @param requestID an identifier for this request (used for tracing)
	 * @param ccbin the input object tree
	 * @param ccbout the output object tree
	 * @throws HostInvokerException if invoke fails
	 */
	public final void invoke(
			final String requestID,
			final ICobolComplexBinding ccbin,
			final ICobolComplexBinding ccbout) throws HostInvokerException {
		throw new HostInvokerException(
				"Unsupported method for CICS containers");
	}

	/**
	 * Invoke a container-driven program.
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
		long start = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
 			LOG.debug("Invoke Container started");
		}
		LegStarRequest request =
			createContainerRequest(requestID, mAddress, inParts);
		try {
			mHostAccessStrategy.invoke(request);
		} catch (HostAccessStrategyException e) {
			throw new HostInvokerException(e);
		}
		
		/* The request might have failed */
		if (request.getException() != null) {
			throw new HostInvokerException(request.getException());
		}
		createContainerResponse(request, outParts);
		
		if (LOG.isDebugEnabled()) {
			long end = System.currentTimeMillis();
			LOG.debug("Invoke Container ended. elapse: "
					+ Long.toString(end - start) + " ms");
		}
	}
	
	/**
	 * Creates a request for a container-driven program. This builds a
	 * multi-part request message where each part corresponds to an input
	 * container.
	 * @param requestID a request traceability identifier
	 * @param address the host endpoint
	 * @param inParts the input parts as a map relating containers to
	 * correponding object trees
	 * @return the request ready to be submitted
	 * @throws HostInvokerException if failed to create request
	 */
	private LegStarRequest createContainerRequest(
			final String requestID,
			final LegStarAddress address,
			final Map < String, ICobolComplexBinding > inParts)
			throws HostInvokerException {
		
		List < LegStarMessagePart > dataParts =
			new ArrayList < LegStarMessagePart >();
        for (String containerName : inParts.keySet()) {
	        ICobolComplexBinding ccbin =
	        	(ICobolComplexBinding) inParts.get(containerName);
	        
			/* The program parameters give the container max size */
	        dataParts.add(createContainerPart(containerName,
					mCicsProgram.getInContainers().get(containerName), ccbin));
        }
        
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
	 * Create a message part for a container.
	 * @param containerName the target CICS container name
	 * @param containerSize the maximum container size
	 * @param ccbin the input JAXB object tree
	 * @return a message part
	 * @throws MarshalException if object tree fails to convert
	 */
	private LegStarMessagePart createContainerPart(final String containerName,
			final int containerSize, final ICobolComplexBinding ccbin)
			throws MarshalException {

		/* Allocate a host data buffer for the part */
		byte[] hostBytes = new byte[containerSize];

		/* Convert from java to host */
		marshal(ccbin, hostBytes);

		/* Create a container part */
		LegStarMessagePart part = new ContainerPart(containerName, hostBytes);

		return part;
	}
	
	/**
	 * The response message is assumed to be multi-part. Each part 
	 * corresponds to an output CICS container. This is not guaranteed to fill
	 * in all expected output parts. Only those which were actually returned
	 * from the host will be present.
	 * @param request the request that was just processed by the host
	 * @param outParts the output parts as a set of JAXB object trees
	 * @throws UnmarshalException if failed to convert host data
	 */
	private void createContainerResponse(
			final LegStarRequest request,
			final Map < String, ICobolComplexBinding > outParts)
			throws UnmarshalException {
		for (LegStarMessagePart part
				: request.getResponseMessage().getDataParts()) {
			if (part.getContent() != null && part.getContent().length > 0) {
				unmarshal(part.getContent(), outParts.get(part.getID()));
			}
		}
	}

}
