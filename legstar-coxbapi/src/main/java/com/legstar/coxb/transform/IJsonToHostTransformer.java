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

import java.io.Reader;

/**
 * JSON to Host transformers offer the capability to turn an JSON to raw
 * mainframe byte arrays.
 * 
 */
public interface IJsonToHostTransformer {

    /**
     * Transforms JSON to host data with a specific host character set.
     * 
     * @param source the JSON Reader to unmarshal JSON data from
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Reader source, final String hostCharset)
            throws HostTransformException;

    /**
     * Transforms JSON to host data.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Reader reader) throws HostTransformException;

    /**
     * Transforms JSON to host data with a specific host character set.
     * 
     * @param source the JSON Reader to unmarshal JSON data from
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Reader source, final String hostCharset,
            final HostTransformStatus status)
            throws HostTransformException;

    /**
     * Transforms JSON to host data.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @return a byte array with host data
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Reader reader,
            final HostTransformStatus status) throws HostTransformException;

}
