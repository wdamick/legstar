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
package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.transform.AbstractTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Groups a couple of transformers for dynamic Java to Host and
 * Host to Java transformations.
 *
 */
public class ReflectTransformers extends AbstractTransformers {

    /**
     * Construct a transformer for a particular JAXB type.
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws ReflectBindingException 
     * @throws ReflectBindingException if JAXB type has no annotations or cannot be 
     *  located from the classpath
     */
    public ReflectTransformers(
            final String jaxbPackageName,
            final String jaxbType) throws ReflectBindingException {
        super(new ReflectJavaToHostTransformer(jaxbPackageName, jaxbType),
                new ReflectHostToJavaTransformer(jaxbPackageName, jaxbType));
    }

    /**
     * Transforms java data object to host data with a specific host character set.
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject, final String hostCharset)
            throws HostTransformException {
        return getJavaToHost().transform(valueObject, hostCharset);
    }

    /**
     * Transforms java data object to host data.
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject)
            throws HostTransformException {
        return getJavaToHost().transform(valueObject);
    }

    /**
     * Transforms host data to java data object with a specific host character set.
     * @param hostData a byte array containing host data
     * @param hostCharset the host character set
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    public Object toJava(final byte[] hostData, final String hostCharset)
            throws HostTransformException {
        return getHostToJava().transform(hostData, hostCharset);
    }

    /**
     * Transforms host data to java data object.
     * @param hostData a byte array containing host data
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    public Object toJava(final byte[] hostData)
            throws HostTransformException {
        return getHostToJava().transform(hostData);
    }

}
