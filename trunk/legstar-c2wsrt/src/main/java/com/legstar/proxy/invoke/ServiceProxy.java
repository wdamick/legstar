/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.proxy.invoke;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.util.Utils;
import com.legstar.messaging.HostMessageFormatException;
import com.legstar.messaging.LegStarMessage;

/**
 * Proxy service Cultureinfo.
 * <p/>
 * This proxy receives raw host data originating from a mainframe, it determines
 * its format (encapsulated in a LegStarMessage or not) and then calls the
 * appropriate operation proxy invoker.
 *
 */
public class ServiceProxy implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = 9222944155913546563L;

    /** Configuration property giving the operation proxy supported. */ 
    public static final String OPERATION_PROXY_CLASS_NAME_PROPERTY =
        "operationProxyClassName";

    /** Default operation proxy. */ 
    public static final String DEFAULT_OPERATION_PROXY_CLASS_NAME =
        "com.legstar.proxy.invoke.ReflectOperationProxy";

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(ServiceProxy.class);
    
    /**
     * Current set of configuration parameters. These are setup at construction
     * time but can be overridden on a per-request basis.
     */
    private Map < String, String > mConfig;


    /** The remote operation supported by this service.*/
    private IOperationProxy mOperationProxy;

    /**
     * Create a service proxy and its inner operation proxy.
     * @param config the initial set of parameters
     * @throws ProxyConfigurationException if configuration is invalid
     */
    public ServiceProxy(
            final Map < String, String > config) throws ProxyConfigurationException {
        if (config == null) {
            mConfig = new HashMap < String, String >();
        } else {
            mConfig = config;
        }
        mOperationProxy = getOperationProxy(mConfig);
    }

    /**
     * Load the operation proxy named in the configuration or the default one
     * if none is found.
     * @param config the current configuration
     * @return an instance of the operation proxy
     * @throws ProxyConfigurationException if unable to instantiate the operation proxy
     */
    private IOperationProxy getOperationProxy(
            final Map < String, String > config)  throws ProxyConfigurationException {
        try {
            String operationProxyClassName = config.get(
                    OPERATION_PROXY_CLASS_NAME_PROPERTY);
            if (operationProxyClassName == null 
                    || operationProxyClassName.length() == 0) {
                operationProxyClassName = DEFAULT_OPERATION_PROXY_CLASS_NAME;
            }
            Class < ? > clazz = Utils.loadClass(operationProxyClassName);
            Constructor < ? > constructor = clazz.getConstructor(Map.class);
            return (IOperationProxy) constructor.newInstance(new Object[] {config});
        } catch (SecurityException e) {
            throw new ProxyConfigurationException(e);
        } catch (IllegalArgumentException e) {
            throw new ProxyConfigurationException(e);
        } catch (ClassNotFoundException e) {
            throw new ProxyConfigurationException(e);
        } catch (NoSuchMethodException e) {
            throw new ProxyConfigurationException(e);
        } catch (InstantiationException e) {
            throw new ProxyConfigurationException(e);
        } catch (IllegalAccessException e) {
            throw new ProxyConfigurationException(e);
        } catch (InvocationTargetException e) {
            if (e.getTargetException() instanceof ProxyConfigurationException) {
                throw (ProxyConfigurationException) e.getTargetException();
            }
            throw new ProxyConfigurationException(e);
        }
    }

    /**
     * Extract raw mainframe data from message envelope if any and then hand over
     * control to an operation proxy invoker.
     * <p/>
     * In the future this will be enhanced to support multiple operations but for
     * now we are limited to one so there is no logic to select an available
     * invoker.
     * @param config request time configuration parameters. This is meant for
     *  things such as credentials
     * @param requestID a unique identifier for the request
     * @param requestBytes the mainframe request data
     * @return reply data ready for transmission to mainframe
     * @throws ProxyInvokerException if invoke fails
     */
    public byte[] invoke(
            final Map < String, String > config,
            final String requestID,
            final byte[] requestBytes) throws ProxyInvokerException {

        try {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Servicing proxy request " + requestID);
            }

            /* Detect if client is using LegStar messaging. */
            boolean legstarMessaging = false;
            byte[] payload = requestBytes;
            if (LegStarMessage.isLegStarMessage(requestBytes)) {
                legstarMessaging = true;
                payload = LegStarMessage.getContentFromHostBytes(requestBytes);
            }
            /* Invoke the requested operation. Note that we only support
             * single method services so far. */
            byte[] replyBytes =
                getOperationProxy().invoke(config, requestID, payload);

            /* If client uses LegStar message format a reply*/
            if (legstarMessaging) {
                return LegStarMessage.getHostBytesFromContent(replyBytes);
            } else {
                return replyBytes;
            }

        } catch (UnsupportedEncodingException e) {
            throw new ProxyInvokerException(e);
        } catch (HostMessageFormatException e) {
            throw new ProxyInvokerException(e);
        }

    }

    /**
     * Same method using default configuration.
     * @param requestID a unique identifier for the request
     * @param requestBytes the mainframe request data
     * @return reply data ready for transmission to mainframe
     * @throws ProxyInvokerException if invoke fails
     */
    public byte[] invoke(
            final String requestID,
            final byte[] requestBytes) throws ProxyInvokerException {
        return invoke(getConfig(), requestID, requestBytes);
    }

    /**
     * @return the current set of configuration parameters
     */
    public Map < String, String > getConfig() {
        return mConfig;
    }

    /**
     * @return the operation proxy
     */
    public IOperationProxy getOperationProxy() {
        return mOperationProxy;
    }

    /**
     * @param operationProxy the operation proxy set
     */
    public void setCultureinfoProxyInvoker(
            final IOperationProxy operationProxy) {
        mOperationProxy = operationProxy;
    }

}
