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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.host.access.HostAccessStrategy;
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.HostMessageFormatException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.impl.LegStarMessageImpl;

/**
 * A ContainerInvoker builds a request object for a target CHANNEL-driven CICS
 * program. The request messages will be multi-part where each part corresponds
 * to a CICS CONTAINER.
 *
 */
public class ContainerInvoker extends AbstractInvoker {

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
        super(hostAccessStrategy, completeAddress, hostProgram);
    }

    /**
     * @deprecated
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
     * @deprecated
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
        try {
            LegStarMessageImpl requestMessage = new LegStarMessageImpl(
                    getProgramAttr().getProgramAttrMap());
            
            /* A new message part is built from the input bindings */
            Iterator < Map.Entry < String, ICobolComplexBinding >>  iterator = inParts.entrySet().iterator();
            while (iterator.hasNext()) {
                Map.Entry < String, ICobolComplexBinding > entry = iterator.next();
                requestMessage.addMessagePart(
                        entry.getValue(),
                        getProgramAttr().getInContainers().get(entry.getKey()),
                        getAddress().getHostCharset(),
                        entry.getKey());
            }
            
            LegStarMessage responseMessage = invoke(requestID, requestMessage);
            
            LegStarMessageImpl responseMessageImpl =
                new LegStarMessageImpl(responseMessage);

            responseMessageImpl.getBindingsFromParts(
                    outParts, getAddress().getHostCharset());

        } catch (HeaderPartException e) {
            throw new HostInvokerException(e);
        } catch (HostMessageFormatException e) {
            throw new HostInvokerException(e);
        }

    }

    /** {@inheritDoc} */
    public byte[] invoke(final String requestID, final byte[] requestBytes) throws HostInvokerException {
        throw new HostInvokerException(
        "Unsupported method for CICS containers");
    }

    /** {@inheritDoc} */
    public Map < String, byte[] > invoke(final String requestID,
            final Map < String, byte[] > requestParts) throws HostInvokerException {
        try {
            LegStarMessage requestMessage = new LegStarMessage();
            requestMessage.setHeaderPart(new LegStarHeaderPart(getProgramAttr().getProgramAttrMap(), 0));
            Iterator < Map.Entry < String, byte[] >> iterator = requestParts.entrySet().iterator();
            while (iterator.hasNext()) {
                Map.Entry < String, byte[] > entry = iterator.next();
                requestMessage.addDataPart(new ContainerPart(entry.getKey(), entry.getValue()));
            }

            LegStarMessage responseMessage = invoke(requestID, requestMessage);

            if (responseMessage == null) {
                return null;
            }
            Map < String, byte[] > response = new HashMap < String, byte[] >();
            for (LegStarMessagePart part : responseMessage.getDataParts()) {
                response.put(part.getPartID(), part.getContent());
            }
            return response;
            
        } catch (HeaderPartException e) {
            throw new HostInvokerException(e);
        }
    }

}
