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
package com.legstar.host.invoke;

import java.util.Iterator;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.host.access.HostAccessStrategy;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.messaging.HostMessageFormatException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.impl.LegStarMessageImpl;

/**
 * A ContainerInvoker builds a request object for a target CHANNEL-driven CICS
 * program. The request messages will be multi-part where each part corresponds
 * to a CICS CONTAINER.
 *
 */
public class ContainerInvoker implements HostInvoker {

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
     * @param completeAddress the host address where all fields are valued
     *  including credentials
     * @param hostProgram the host program attributes
     * @throws HostInvokerException in construction fails
     */
    public ContainerInvoker(
            final HostAccessStrategy hostAccessStrategy,
            final LegStarAddress completeAddress,
            final CicsProgram hostProgram) throws HostInvokerException {
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
        try {
            LegStarRequest request = createContainerRequest(
                    requestID, mAddress, inParts);
            mHostAccessStrategy.invoke(request);

            /* The request might have failed */
            if (request.getException() != null) {
                throw new HostInvokerException(request.getException());
            }
            createContainerResponse(request, outParts);

        } catch (HostAccessStrategyException e) {
            throw new HostInvokerException(e);
        }

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
     * corresponding object trees
     * @return the request ready to be submitted
     * @throws HostInvokerException if failed to create request
     */
    private LegStarRequest createContainerRequest(
            final String requestID,
            final LegStarAddress address,
            final Map < String, ICobolComplexBinding > inParts)
    throws HostInvokerException {

        /* Construct a LegStar message, passing the host program attributes */
        try {
            LegStarMessageImpl requestMessage = new LegStarMessageImpl(
                    mCicsProgram.getProgramAttrMap());

            /* A new message part is built from the input bindings */
            Iterator < Map.Entry < String, ICobolComplexBinding >>
            keyValuePairs = inParts.entrySet().iterator();
            for (int i = 0; i < inParts.size(); i++) {
                Map.Entry < String, ICobolComplexBinding > entry =
                    (Map.Entry < String, ICobolComplexBinding >)
                    keyValuePairs.next();
                ICobolComplexBinding ccbin = entry.getValue();
                requestMessage.addMessagePart(
                        ccbin,
                        mCicsProgram.getInContainers().get(entry.getKey()),
                        mAddress.getHostCharset(),
                        entry.getKey());
            }
            return new LegStarRequest(requestID, address, requestMessage);
        } catch (HeaderPartException e) {
            throw new HostInvokerException(e);
        } catch (HostMessageFormatException e) {
            throw new HostInvokerException(e);
        }
    }

    /**
     * The response message is assumed to be multi-part. Each part 
     * corresponds to an output CICS container. This is not guaranteed to fill
     * in all expected output parts. Only those which were actually returned
     * from the host will be present.
     * @param request the request that was just processed by the host
     * @param outParts the output parts as a set of JAXB object trees
     * @throws HostInvokerException if failed to convert host data
     */
    private void createContainerResponse(
            final LegStarRequest request,
            final Map < String, ICobolComplexBinding > outParts)
    throws HostInvokerException {

        try {
            LegStarMessageImpl responseMessage =
                new LegStarMessageImpl(request.getResponseMessage());

            responseMessage.getBindingsFromParts(
                    outParts, mAddress.getHostCharset());

        } catch (HeaderPartException e) {
            throw new HostInvokerException(e);
        } catch (HostMessageFormatException e) {
            throw new HostInvokerException(e);
        }

    }

}
