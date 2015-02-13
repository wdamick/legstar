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
import java.io.Writer;

/**
 * Alternative to {@link IHostTransformers} which works on JSON rather than java
 * object.
 * <p/>
 * Classes implementing this interface can transform JSON to mainframe data
 * streams and vice versa.
 * 
 */
public interface IHostJsonTransformers {

    /**
     * Transforms JSON source to host data with a specific host character set.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(
            final Reader reader, final String hostCharset)
            throws HostTransformException;

    /**
     * Transforms JSON source to host data.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(final Reader reader) throws HostTransformException;

    /**
     * Transforms host data to JSON using a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    void toJson(final byte[] hostData, final Writer writer,
            final String hostCharset)
            throws HostTransformException;

    /**
     * Transforms host data to JSON.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    void toJson(final byte[] hostData, final Writer writer)
            throws HostTransformException;

    /**
     * Transforms JSON source to host data with a specific host character set.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(
            final Reader reader, final String hostCharset,
            final HostTransformStatus status)
            throws HostTransformException;

    /**
     * Transforms JSON source to host data.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(final Reader reader,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms host data to JSON using a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    void toJson(final byte[] hostData, final Writer writer,
            final String hostCharset,
            final HostTransformStatus status)
            throws HostTransformException;

    /**
     * Transforms host data to JSON.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    void toJson(final byte[] hostData, final Writer writer,
            final HostTransformStatus status)
            throws HostTransformException;

}
