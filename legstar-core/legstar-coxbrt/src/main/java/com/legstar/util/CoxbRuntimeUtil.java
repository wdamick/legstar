/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.util;

import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.util.BindingUtil;
import com.legstar.coxb.util.ClassLoadingException;
import com.legstar.coxb.util.ClassUtil;
import com.legstar.coxb.host.HostException;

/**
 * Utility methods used for JAXB object tree introspection.
 * 
 * @author Fady Moussallam
 * 
 * 
 */
public final class CoxbRuntimeUtil {

    /**
     * Private constructor to stop anyone from instantiating
     * this class - the static methods should be used
     * explicitly.
     */
    private CoxbRuntimeUtil() {

    }

    /**
     * This function returns a byte length for a complex object as a String.
     * 
     * @param jaxbPackage the java package name from which an ObjectFactory
     *            can be instanciated
     * @param jaxbTypeName the JAXB type name of the object for which byte
     *            length must be returned
     * @return the byte length as a string
     * @throws HostException if byte length calculation failed
     */
    public static String byteLength(
            final String jaxbPackage,
            final String jaxbTypeName)
            throws HostException {

        try {
            /* Load the JAXB object factory from the package */
            Object objectFactory = ClassUtil.newObject(jaxbPackage,
                    "ObjectFactory");

            /* Get an instance of the requested JAXB object */
            Object jaxbObject = BindingUtil.newJaxbObject(objectFactory,
                    jaxbTypeName);

            /* Create a complex cobol element representing this jaxb object */
            CComplexReflectBinding ce =
                    new CComplexReflectBinding(objectFactory, jaxbObject);
            return (Integer.toString(ce.calcByteLength()));
        } catch (ClassLoadingException e) {
            throw (new HostException(e));
        }
    }

}
