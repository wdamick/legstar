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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.host.access.HostAccessStrategy;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.host.invoke.model.HostProgram;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;

/**
 * Abstract invoker.
 * <p/>
 * An invoker has the capability to run a host program. The host program
 * attributes are loaded from a properties files and an XML configuration 
 * is expected to provide host access parameters.
 * <p/>
 * The actual transport used is abstracted by dynamically
 * loading a host accessor based on a factory name from the XML configuration.
 */
public abstract class AbstractInvoker implements HostInvoker {

    /** Direct or Pooled host access strategy. */
    private HostAccessStrategy mHostAccessStrategy;

    /** Host endpoint targeted. */
    private LegStarAddress mAddress;

    /** Host program attributes. */
    private HostProgram mHostProgram;

    /** Logger. */
    private final Log _log = LogFactory.getLog(AbstractInvoker.class);

    /**
     * All invokers share this constructor.
     * @param hostAccessStrategy the pooled or direct access configured
     * @param completeAddress the host address where all fields are valued
     *  including credentials
     * @param hostProgram the host program attributes
     * @throws HostInvokerException in construction fails
     */
    public AbstractInvoker(
            final HostAccessStrategy hostAccessStrategy,
            final LegStarAddress completeAddress,
            final HostProgram hostProgram) throws HostInvokerException {
        mHostAccessStrategy = hostAccessStrategy;
        mAddress = completeAddress;
        mHostProgram = hostProgram;
    }

    /**
     * Generic mainframe invoke method.
     * <p/>
     * Caller must use LegStar Messaging.
     * @param requestID the request ID
     * @param requestMessage the formatted request message
     * @return a formatted reply
     * @throws HostInvokerException if invoke fails
     */
    public LegStarMessage invoke(final String requestID,
            final LegStarMessage requestMessage) throws HostInvokerException {
        
        long start = System.currentTimeMillis();
        if (_log.isDebugEnabled()) {
            _log.debug("Invoke mainframe started");
        }
        
        /* Create a request instance and call the host program */
        LegStarRequest request = new LegStarRequest(
                requestID, getAddress(), requestMessage);
        try {
            getHostAccessStrategy().invoke(request);
        } catch (HostAccessStrategyException e) {
            throw new HostInvokerException(e);
        }

        /* The request might have failed */
        if (request.getException() != null) {
            throw new HostInvokerException(request.getException());
        }

        if (_log.isDebugEnabled()) {
            long end = System.currentTimeMillis();
            _log.debug("Invoke mainframe ended. elapse: "
                    + Long.toString(end - start) + " ms");
        }

        return request.getResponseMessage();
    }

    /**
     * @return the host address
     */
    public LegStarAddress getAddress() {
        return mAddress;
    }

    /**
     * @return the host access strategy
     */
    public HostAccessStrategy getHostAccessStrategy() {
        return mHostAccessStrategy;
    }

    /**
     * @return the host program attributes
     */
    public HostProgram getHostProgram() {
        return mHostProgram;
    }
    
    /**
     * @return the host program attributes
     */
    public HostProgramProperties getProgramAttr() {
        return null;
    }

    /** {@inheritDoc} */
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("HostInvoker=[");
        if (getAddress() != null) {
            sb.append(getAddress().toString() + ",");
        }
        sb.append("HostAccessStrategy=" + getHostAccessStrategy().getClass().getName());
        sb.append("," +  getHostProgram().toString());
        sb.append("]");
        return sb.toString();
    }
}
