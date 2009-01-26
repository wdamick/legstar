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
package com.legstar.coxb.transform;


/**
 * Classes implementing this interface group transformers for a given
 * complex type.
 *
 */
public interface IHostTransformers {

    /**
     * Transforms java data object to host data with a specific host character set.
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(
            final Object valueObject, final String hostCharset) throws HostTransformException;

    /**
     * Transforms java data object to host data.
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(final Object valueObject) throws HostTransformException;

    /**
     * Transforms host data to java data object with a specific host character set.
     * @param hostData a byte array containing host data
     * @param hostCharset the host character set
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    Object toJava(final byte[] hostData, final String hostCharset)
            throws HostTransformException;

    /**
     * Transforms host data to java data object.
     * @param hostData a byte array containing host data
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    Object toJava(final byte[] hostData) throws HostTransformException;

    /**
     * @return the transformer that turns a java data object into host data
     */
    IJavaToHostTransformer getJavaToHost();

    /**
     * @param javaToHost the the transformer that turns a java data object into host data to set
     */
    void setJavaToHost(final IJavaToHostTransformer javaToHost);

    /**
     * @return the transformer that turns host data into a java data object
     */
    IHostToJavaTransformer getHostToJava();

    /**
     * @param hostToJava the transformer that turns host data into a java data object to set
     */
    void setHostToJava(final IHostToJavaTransformer hostToJava);

}
