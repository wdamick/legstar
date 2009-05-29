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
package com.legstar.proxy.invoke;

import java.util.Map;

import com.legstar.coxb.host.HostContext;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.coxb.transform.IHostTransformers;

/**
 * Generic code for proxy operations.
 *
 */
public abstract class AbstractOperationProxy implements IOperationProxy {

    /** An implementation of a proxy invoker. */
    private IProxyInvoker mProxyInvoker;

    /** Host transformers for request java data object. */
    private IHostTransformers mRequestTransformers;

    /** Host transformers for response java data object. */
    private IHostTransformers mResponseTransformers;
    
    /** The current configuration parameter set. */
    private Map < String, String > mConfig;
    
    /**
     * All operation proxies must implement a constructor that
     * takes configuration parameters.
     * @param config the initial set of parameters
     * @throws ProxyConfigurationException if configuration is invalid
     */
    public AbstractOperationProxy(
            final Map < String, String > config) throws ProxyConfigurationException {
        mConfig = config;
    }

    /** {@inheritDoc} */
    public byte[] invoke(
            final String requestID,
            final byte[] requestBytes) throws ProxyInvokerException {
        return invoke(getConfig(), requestID, requestBytes);
        
    }
    
    /** {@inheritDoc} */
    public byte[] invoke(
            final Map < String, String > config,
            final String requestID,
            final byte[] requestBytes) throws ProxyInvokerException {
        
        try {
            IProxyInvoker proxyInvoker = getProxyInvoker(config);
            String hostCharset = getHostCharset(config);

            Object requestObject =
                getRequestTransformers().toJava(requestBytes, hostCharset);

            Object replyObject = proxyInvoker.invoke(requestID, requestObject);

            return getResponseTransformers().toHost(replyObject, hostCharset);
            
        } catch (HostTransformException e) {
            throw new ProxyInvokerException(e);
        }
        
    }

   /**
     * Create a new invoker either because we don't have any of because the
     * configuration parameters are different.
     * @param config a set of configuration parameters
     * @return the new proxy invoker
     * @throws ProxyInvokerException if proxy invoker cannot be created
     */
    public IProxyInvoker getProxyInvoker(
            final Map < String, String > config) throws ProxyInvokerException {
        if (config == null) {
            if (mProxyInvoker == null) {
                throw new ProxyInvokerException("No configuration was set");
            } else {
                return mProxyInvoker;
            }
        }
        if (mProxyInvoker == null || !mProxyInvoker.isSameConfig(config)) {
            mProxyInvoker = ProxyInvokerFactory.createProxyInvoker(config);
        }
        return mProxyInvoker;
    }

    /**
     * @param config the current configuration set
     * @return the host character set from the configuration if any, a
     *  default one otherwise
     */
    public String getHostCharset(final Map < String, String > config) {
        String hostCharset = config.get(HOST_CHARSET_PROPERTY);
        if (hostCharset == null || hostCharset.length() == 0) {
            hostCharset = HostContext.getDefaultHostCharsetName();
        }
        return hostCharset;
    }


    /**
     * @return the proxy invoker
     */
    public IProxyInvoker getProxyInvoker() {
        return mProxyInvoker;
    }

    /**
     * @param proxyInvoker the proxy invoker to set
     */
    public void setProxyInvoker(final IProxyInvoker proxyInvoker) {
        mProxyInvoker = proxyInvoker;
    }
    /**
     * @return the host transformers for request java data object
     */
    public IHostTransformers getRequestTransformers() {
        return mRequestTransformers;
    }

    /**
     * @param requestTransformers the host transformers for request java data object to set
     */
    public void setRequestTransformers(
            final IHostTransformers requestTransformers) {
        mRequestTransformers = requestTransformers;
    }

    /**
     * @return the host transformers for response java data object
     */
    public IHostTransformers getResponseTransformers() {
        return mResponseTransformers;
    }

    /**
     * @param responseTransformers the host transformers for response java data object to set
     */
    public void setResponseTransformers(
            final IHostTransformers responseTransformers) {
        mResponseTransformers = responseTransformers;
    }

    /**
     * {@inheritDoc}
     */
    public Map < String, String > getConfig() {
        return mConfig;
    }

    /**
     * {@inheritDoc}
     */
    public void setConfig(final Map < String, String > config) {
        mConfig = config;
    }

    /**
     * Retrieve a mandatory configuration parameter value.
     * @param parmName configuration parameter name
     * @return the parameter value
     * @throws ProxyConfigurationException if parameter is missing
     */
    public String getConfigParm(
            final String parmName) throws ProxyConfigurationException {
        String value = getConfig().get(parmName);
        if (value == null || value.length() == 0) {
            throw new ProxyConfigurationException(
                    "Missing configuration parameter " + parmName);
        }
        return value;
    }

}
