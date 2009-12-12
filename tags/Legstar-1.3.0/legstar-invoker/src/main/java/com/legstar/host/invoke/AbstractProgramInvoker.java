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

import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.host.invoke.model.HostProgram;
import com.legstar.host.invoke.model.HostProgramException;
import com.legstar.messaging.LegStarAddress;

/**
 * Generic code shared by all program invoker implementations. Such invokers
 * have the capability to invoke a mainframe program.
 * <p/>
 * Typically Adapters will implement such an invoker for each operation that
 * maps to a mainframe program.
 *
 */
public abstract class AbstractProgramInvoker {

    /** The operation name.*/
    private String mOperationName;

    /** Host program properties for operation. */
    private String  mProgramProperties;
    
    /** Host program associated with operation. */
    private HostProgram  mHostProgram;

    /** Each target address needs its own host invoker. There is a serious
     * performance hit each time we create a host invoker so we use this
     * cache to improve that. */
    private ConcurrentMap < LegStarAddress, HostInvoker > _hostInvokersCache;

    /** The invoker configuration file name. */
    private String mConfigFileName;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Operation invoker constructor.
     * @param configFileName host invoker configuration file name
     * @param operationName operation name
     * @param programProperties host program properties
     * throws IllegalArgumentException if properties file not found or invalid
     */
    public AbstractProgramInvoker(
            final String configFileName,
            final String operationName,
            final String programProperties) {
        this(configFileName, operationName, (HostProgram) null);
        try {
            mHostProgram = new HostProgramProperties(programProperties);
        } catch (HostProgramException e) {
            throw new IllegalArgumentException(e);
        }
        mProgramProperties = programProperties;
    }

    /**
     * Operation invoker constructor.
     * @param configFileName host invoker configuration file name
     * @param operationName operation name
     * @param hostProgram host program bean associated with operation 
     */
    public AbstractProgramInvoker(
            final String configFileName,
            final String operationName,
            final HostProgram hostProgram) {
        mConfigFileName = configFileName;
        mOperationName = operationName;
        mHostProgram = hostProgram;
        _hostInvokersCache = new ConcurrentHashMap < LegStarAddress, HostInvoker >();
    }

    /** {@inheritDoc} */
    public String toString() {
        StringBuffer details = new StringBuffer();
        details.append("Operation=" + getOperationName());
        details.append(", Program properties=" + getHostProgram());
        details.append(", Config file name=" + getConfigFileName());
        for (Entry < LegStarAddress, HostInvoker > entry : _hostInvokersCache.entrySet()) {
            details.append(", " + entry.getValue().toString());
        }
        return details.toString();
    }

    /**
     * Creates a new host invoker either because there is none yet or because something
     * in the request supersedes the previous address parameters.
     * @param address the target host address (potentially null, meaning the default
     * configuration endpoint is to be used).
     * @return a host invoker
     * @throws HostInvokerException if invoker cannot be created
     */
    public HostInvoker getHostInvoker(final LegStarAddress address) throws HostInvokerException {
        /* Concurrent hash map does not like null value as a key so key with empty
         * address instead of null. */
        LegStarAddress keyAddress = (address == null) ? new LegStarAddress((String) null) : address;
        HostInvoker hostInvoker = _hostInvokersCache.get(keyAddress);
        if (hostInvoker == null) {
            if (_log.isDebugEnabled()) {
                _log.debug("Creating new host invoker for keyAddress: " + keyAddress);
            }
            HostInvoker newHostInvoker = HostInvokerFactory.createHostInvoker(
                    getConfigFileName(), keyAddress, getHostProgram());
            hostInvoker = _hostInvokersCache.putIfAbsent(keyAddress, newHostInvoker);
            if (hostInvoker == null) {
                hostInvoker = newHostInvoker;
            }
            
        }
        return hostInvoker;
    }

    /**
     * @return the invoker configuration file name
     */
    public String getConfigFileName() {
        return mConfigFileName;
    }

    /**
     * @deprecated
     * @return the host program properties for operation
     */
    public String getProgramProperties() {
        return mProgramProperties;
    }

    /**
     * @return the host program properties for operation
     */
    public HostProgram getHostProgram() {
        return mHostProgram;
    }

    /**
     * @return the operation name
     */
    public String getOperationName() {
        return mOperationName;
    }

}
