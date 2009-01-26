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
package com.legstar.proxy.invoke.pojo;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.util.Utils;
import com.legstar.proxy.invoke.AbstractProxyInvoker;
import com.legstar.proxy.invoke.ProxyInvokerException;

/**
 * This provides the capability to call a POJO method.
 * <p/>
 * The POJO is assumed to implement the Data Transfer Object/Remote facade patterns
 * {@link http://java.sun.com/blueprints/corej2eepatterns/Patterns/TransferObject.html}.
 * <p/>
 * The class is immutable. All parameters are passed at construction time.
 * Limitations:
 * <ul>
 *<li>POJO must implement a no-argument constructor</li>
 *<li>Service.Mode.PAYLOAD JAXB Objects are used to create the SOAP payload</li>
 *<li>No support for authentication against target Web Service</li>
 * </ul> 
 *
 */
public class PojoInvoker extends AbstractProxyInvoker {

    /* ====================================================================== */
    /* = Constants section                                                  = */
    /* ====================================================================== */
    /** POJO Class name. */ 
    public static final String POJO_CLASS_NAME_PROPERTY = "pojoClassName";

    /** POJO method name. */ 
    public static final String POJO_METHOD_NAME_PROPERTY = "pojoMethodName";

    /* ====================================================================== */
    /* = Properties section                                                 = */
    /* ====================================================================== */
    /** The POJO Class name. */
    private String mPojoClassName;

    /** The POJO method name. */
    private String mPojoMethodName;
    
    /** The POJO class type. */
    private Class < ? > mPojoClass;

    /** The POJO method. */
    private Method mPojoMethod;

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(PojoInvoker.class);

    /**
     * Standard constructor. The configuration parameters supported are:
     * <ul>
     *  <li>pojoClassName: Target POJO Class name</li>
     *  <li>pojoMethodName: Target POJO method name</li>
     * </ul>
     * @param config configuration parameters
     * @throws PojoInvokerException if configuration is wrong 
     */
    public PojoInvoker(
            final Map < String, String > config) throws PojoInvokerException {
        
        super(config);
        
        mPojoClassName = config.get(POJO_CLASS_NAME_PROPERTY);
        if (mPojoClassName == null || mPojoClassName.length() == 0) {
            throw new PojoInvokerException("You must specify a POJO class name using the "
                    + POJO_CLASS_NAME_PROPERTY + " attribute");
        }
        mPojoMethodName = config.get(POJO_METHOD_NAME_PROPERTY);
        if (mPojoMethodName == null || mPojoMethodName.length() == 0) {
            throw new PojoInvokerException("You must specify a POJO method name using the "
                    + POJO_METHOD_NAME_PROPERTY + " attribute");
        }
        
        try {
            mPojoClass = Utils.loadClass(mPojoClassName);
            boolean methodFound = false;
            for (Method method : mPojoClass.getMethods()) {
                if (method.getName().equals(mPojoMethodName)) {
                    mPojoMethod = method;
                    methodFound = true;
                    break;
                }
            }
            if (!methodFound) {
                throw new PojoInvokerException("Class " + mPojoClassName
                        + " does not implement method " + mPojoMethodName);
            }
        } catch (ClassNotFoundException e) {
            throw new PojoInvokerException(e);
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("PojoInvoker setup configuration:");
            LOG.debug("POJO class name=" + getPojoClassName());
            LOG.debug("POJO method name=" + getPojoMethodName());
        }
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public < T > T invoke(final String requestID, final Object oRequest) throws ProxyInvokerException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("About to call method " + getPojoMethodName() + " for POJO="
                    + getPojoClassName() + " request ID=" + requestID);
        }
        Object replyObject;
        try {
            Object processor = getPojoClass().newInstance();
            replyObject = getPojoMethod().invoke(processor, new Object[] {oRequest});
        } catch (IllegalArgumentException e) {
            throw new ProxyInvokerException(e);
        } catch (InstantiationException e) {
            throw new ProxyInvokerException(e);
        } catch (IllegalAccessException e) {
            throw new ProxyInvokerException(e);
        } catch (InvocationTargetException e) {
            throw new ProxyInvokerException(e);
        }
        if (LOG.isDebugEnabled()) {
            LOG.debug("Returned from method " + getPojoMethodName() + " for POJO="
                    + getPojoClassName() + " request ID=" + requestID);
        }
        return (T) replyObject;
    }

    /**
     * @return the POJO Class name
     */
    public final String getPojoClassName() {
        return mPojoClassName;
    }

    /**
     * @return the POJO method name
     */
    public final String getPojoMethodName() {
        return mPojoMethodName;
    }

    /**
     * @return the POJO class type
     */
    public Class < ? > getPojoClass() {
        return mPojoClass;
    }

    /**
     * @return the POJO method
     */
    public Method getPojoMethod() {
        return mPojoMethod;
    }

}
