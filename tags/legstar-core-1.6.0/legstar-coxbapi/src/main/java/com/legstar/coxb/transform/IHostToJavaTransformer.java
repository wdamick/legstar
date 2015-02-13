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
package com.legstar.coxb.transform;

/**
 * Host to Java transformers offer the capability to turn a raw mainframe byte
 * array
 * to a java data object.
 * 
 */
public interface IHostToJavaTransformer extends IHostTransformer {

    /**
     * Transforms host data to java with a specific host character set.
     * 
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param hostCharset the host character set
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData, final String hostCharset)
            throws HostTransformException;

    /**
     * Transforms host data to java with a specific host character set.
     * 
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param hostCharset the host character set
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData, final int offset,
            final String hostCharset) throws HostTransformException;

    /**
     * Transforms host data to java.
     * 
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData) throws HostTransformException;

    /**
     * Transforms host data to java.
     * 
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData, final int offset)
            throws HostTransformException;

    /**
     * Transforms host data to java with a specific host character set.
     * 
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData, final String hostCharset,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms host data to java with a specific host character set.
     * 
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData, final int offset,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms host data to java.
     * 
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms host data to java.
     * 
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData, final int offset,
            final HostTransformStatus status) throws HostTransformException;
}
