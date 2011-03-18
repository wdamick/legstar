/*******************************************************************************
 * Copyright (c) 2011 LegSem.
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
 * Java to Host transformers offer the capability to turn a java data object
 * to raw mainframe byte arrays.
 * 
 */
public interface IJavaToHostTransformer extends IHostTransformer {

    /**
     * Transforms java to host data with a specific host character set.
     * 
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Object valueObject, final String hostCharset)
            throws HostTransformException;

    /**
     * Transforms java to host data.
     * 
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Object valueObject) throws HostTransformException;

    /**
     * Transforms java to host data with a specific host character set.
     * 
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Object valueObject, final String hostCharset,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms java to host data.
     * 
     * @param valueObject a java value object
     * @return a byte array with host data
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Object valueObject,
            final HostTransformStatus status) throws HostTransformException;

}
