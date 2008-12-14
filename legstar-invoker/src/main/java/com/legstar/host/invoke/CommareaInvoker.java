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
 * An invoker has the capability to run a host program using java object trees
 * as input and output structures. The host program attributes are loaded from
 * a properties files and an XML configuration is expected to provide host
 * access parameters. The actual transport used is abstracted by dynamically
 * loading a host accessor based on a factory name from the XML configuration.
 */
public class CommareaInvoker implements HostInvoker {

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
     * @param completeAddress the host address where all fields are valued
     *  including credentials
     * @param hostProgram the host program attributes
     * @throws HostInvokerException in construction fails
     */
    public CommareaInvoker(
            final HostAccessStrategy hostAccessStrategy,
            final LegStarAddress completeAddress,
            final CicsProgram hostProgram) throws HostInvokerException {
        mHostAccessStrategy = hostAccessStrategy;
        mAddress = completeAddress;
        mCicsProgram = hostProgram;
    }

    /**
     * Invoke a commarea-driven program.
     * @param requestID an identifier for this request (used for tracing)
     * @param ccbin the input binding
     * @param ccbout the output binding
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

        try {
            /* Construct a LegStar message, passing the host program
             *  attributes */
            LegStarMessageImpl requestMessage =
                new LegStarMessageImpl(mCicsProgram.getProgramAttrMap());

            /* A new message part is built from the input binding */
            requestMessage.addMessagePart(
                    ccbin, mCicsProgram.getLength(), mAddress.getHostCharset(),
                    null);

            /* Create a request instance and call the host program */
            LegStarRequest request = new LegStarRequest(
                    requestID, mAddress, requestMessage);
            try {
                mHostAccessStrategy.invoke(request);
            } catch (HostAccessStrategyException e) {
                throw new HostInvokerException(e);
            }

            /* The request might have failed */
            if (request.getException() != null) {
                throw new HostInvokerException(request.getException());
            }

            /* Unwrap the response and convert to a java data object */
            LegStarMessageImpl responseMessage =
                new LegStarMessageImpl(request.getResponseMessage());
            responseMessage.getBindingFromPart(
                    ccbout, mAddress.getHostCharset(), null);

        } catch (HeaderPartException e) {
            throw new HostInvokerException(e);
        } catch (HostMessageFormatException e) {
            throw new HostInvokerException(e);
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
