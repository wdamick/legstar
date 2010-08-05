/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.host.access.HostAccessStrategy;
import com.legstar.host.invoke.model.HostProgram;
import com.legstar.host.invoke.model.HostProgramException;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;


/**
 * An invoker has the capability to run a host program using java object trees
 * as input and output structures. The host program attributes are loaded from
 * a properties files and an XML configuration is expected to provide host
 * access parameters. The actual transport used is abstracted by dynamically
 * loading a host accessor based on a factory name from the XML configuration.
 */
public class CommareaInvoker extends AbstractInvoker {

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
            final HostProgram hostProgram) throws HostInvokerException {
        super(hostAccessStrategy, completeAddress, hostProgram);
    }

    /**
     * @deprecated
     * This method is invalid for Commarea-driven programs. In a future
     * version it will be supported to allow for a commarea to be mapped 
     * to multiple Cobol structures.
     * @param requestID an identifier for this request (used for tracing)
     * @param inParts a set of input object trees with target containers
     * @param outParts a set of output object trees with source containers
     * @throws HostInvokerException if invoke fails
     */
    public void invoke(
            final String requestID,
            final Map < String, ICobolComplexBinding > inParts,
            final Map < String, ICobolComplexBinding > outParts)
    throws HostInvokerException {
        throw new HostInvokerException(
        "Unsupported method for CICS commarea");
    }

    /** {@inheritDoc} */
    public byte[] invoke(final String requestID, final byte[] requestBytes) throws HostInvokerException {
        try {
            LegStarMessage requestMessage = new LegStarMessage();
            requestMessage.setHeaderPart(new LegStarHeaderPart(0, getHostProgram().toJSONHost()));
            requestMessage.addDataPart(new CommareaPart(requestBytes));

            LegStarMessage responseMessage = invoke(requestID, requestMessage);

            if (responseMessage == null) {
                return null;
            }
            if (responseMessage.getDataParts().size() == 0) {
                return new byte[0];
            }
            if (responseMessage.getDataParts().size() > 1) {
                throw new HostInvokerException("Unexpected number of parts "
                        + responseMessage.getDataParts().size()
                        + " in the host response");
            }
            return responseMessage.getDataParts().get(0).getContent();
            
        } catch (HeaderPartException e) {
            throw new HostInvokerException(e);
        } catch (HostProgramException e) {
            throw new HostInvokerException(e);
        }
    }

    /** {@inheritDoc} */
    public Map < String, byte[] > invoke(final String requestID,
            final Map < String, byte[] > requestParts) throws HostInvokerException {
        throw new HostInvokerException(
        "Unsupported method for CICS commarea");
    }

}
