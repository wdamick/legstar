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

import com.legstar.coxb.ICobolComplexBinding;

/**
 * Abstract class to invoke host functions.
 */
public interface HostInvoker {

    /**
     * @deprecated
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
     * @deprecated
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
    
    /**
     * Invoke a host function using one input and one output raw mainframe
     * data buffers.
     * <p/>
     * Caller is in charge of transforming mainframe data to whatever format
     * he needs.
     * @param requestID an identifier for this request (used for tracing)
     * @param requestBytes the request mainframe data byte array
     * @return the reply mainframe data bytes
     * @throws HostInvokerException if invoke fails
     */
    byte[] invoke(
            String requestID,
            byte[] requestBytes) throws HostInvokerException;

    /**
     * Invoke a host function with multiple input and multiple parts.
     * <p/>
     * Parts are named and associated with a raw mainframe data buffer.
     * <p/>
     * Caller is in charge of transforming mainframe data to whatever format
     * he needs.
     * @param requestID an identifier for this request (used for tracing)
     * @param requestParts a map of mainframe data byte arrays
     * @return a map of mainframe data byte arrays
     * @throws HostInvokerException if invoke fails
     */
    Map < String, byte[] > invoke(
            String requestID,
            Map < String, byte[] > requestParts)
    throws HostInvokerException;
}
