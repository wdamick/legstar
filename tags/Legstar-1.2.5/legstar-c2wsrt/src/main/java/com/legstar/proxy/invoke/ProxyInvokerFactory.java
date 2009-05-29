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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;

import com.legstar.coxb.util.Utils;

/**
 * Given a set of configuration parameters, this class creates instances of
 * proxy invokers.
 *
 */
public final class ProxyInvokerFactory {
    
    /** Default operation proxy. */ 
    public static final String DEFAULT_PROXY_INVOKER_CLASS_NAME =
        "com.legstar.proxy.invoke.jaxws.WebServiceInvoker";

    /**
     * Utility class.
     */
    private ProxyInvokerFactory() {
        
    }
    
    /**
     * Locate the proxy invoker using configuration parameters and loading the class
     * from the classpath.
     * <p/>
     * If no proxy invoker is configured we use the web service invoker as the default
     * for backward compatibility.
     * @param config the set of configuration parameters
     * @return an instance of a proxy invoker
     * @throws ProxyInvokerException if invoker cannot be created
     */
    public static IProxyInvoker createProxyInvoker(
            final Map < String, String > config) throws ProxyInvokerException {

        String className = config.get(IProxyInvoker.PROXY_INVOKER_CLASS_NAME_PROPERTY);
        if (className == null || className.length() == 0) {
            className = DEFAULT_PROXY_INVOKER_CLASS_NAME;
        }
        try {
            Class < ? > proxyInvokerClass = Utils.loadClass(className);
            Constructor < ? > constructor = proxyInvokerClass.getConstructor(Map.class);
            return (IProxyInvoker) constructor.newInstance(new Object[] {config});
        } catch (ClassNotFoundException e) {
            throw new ProxyInvokerException(e);
        } catch (SecurityException e) {
            throw new ProxyInvokerException(e);
        } catch (NoSuchMethodException e) {
            throw new ProxyInvokerException(e);
        } catch (IllegalArgumentException e) {
            throw new ProxyInvokerException(e);
        } catch (InstantiationException e) {
            throw new ProxyInvokerException(e);
        } catch (IllegalAccessException e) {
            throw new ProxyInvokerException(e);
        } catch (InvocationTargetException e) {
            throw new ProxyInvokerException(e.getTargetException());
        }
    }

}
