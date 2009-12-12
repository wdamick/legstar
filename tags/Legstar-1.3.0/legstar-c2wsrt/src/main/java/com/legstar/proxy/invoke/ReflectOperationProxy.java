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

import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.coxb.impl.reflect.ReflectTransformers;

/**
 * A dynamic operation proxy.
 * <p/>
 * This class uses configuration parameters to discover operation request
 * and response types. It can be used as a generic alternative to generated
 * operation proxy classes.
 *
 */
public class ReflectOperationProxy extends AbstractOperationProxy {

    /** Serial ID. */
    private static final long serialVersionUID = 1L;

    /** Request JAXB type configuration parameter. */ 
    public static final String REQUEST_JAXB_TYPE_PROPERTY =
        "requestJaxbType";

    /** Request JAXB package name configuration parameter. */ 
    public static final String REQUEST_JAXB_PACKAGE_NAME_PROPERTY =
        "requestJaxbPackageName";

    /** Response JAXB type. */ 
    public static final String RESPONSE_JAXB_TYPE_PROPERTY =
        "responseJaxbType";

    /** Response JAXB package name. */ 
    public static final String RESPONSE_JAXB_PACKAGE_NAME_PROPERTY =
        "responseJaxbPackageName";

    /**
     * Create the proxy invoker for the set of configuration parameters passed.
     * The configuration parameters expected are:
     * <ul>
     *  <li>requestJaxbType: JAXB request type</li>
     *  <li>requestJaxbPackageName: JAXB request package name</li>
     *  <li>responseJaxbType: JAXB response type</li>
     *  <li>responseJaxbPackageName: JAXB response package name</li>
     * </ul>
     * @param config the initial set of parameters
     * @throws ProxyConfigurationException if a JAXB type has no annotations or cannot be 
     *  located from the classpath
     */
    public ReflectOperationProxy(
            final Map < String, String > config) throws ProxyConfigurationException {
        super(config);
        try {
            setRequestTransformers(new ReflectTransformers(
                    getConfigParm(REQUEST_JAXB_PACKAGE_NAME_PROPERTY),
                    getConfigParm(REQUEST_JAXB_TYPE_PROPERTY)));
            setResponseTransformers(new ReflectTransformers(
                    getConfigParm(RESPONSE_JAXB_PACKAGE_NAME_PROPERTY),
                    getConfigParm(RESPONSE_JAXB_TYPE_PROPERTY)));
        } catch (ReflectBindingException e) {
            throw new ProxyConfigurationException(e);
        }
    }
    
}
