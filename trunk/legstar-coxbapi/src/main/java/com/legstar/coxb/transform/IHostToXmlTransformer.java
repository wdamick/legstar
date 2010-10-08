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
package com.legstar.coxb.transform;

import java.io.Writer;

/**
 * Host to XML transformers offer the capability to turn a raw mainframe byte
 * array to XML.
 * 
 */
public interface IHostToXmlTransformer {

    /**
     * Transforms host data to XML with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    void transform(
            final byte[] hostData,
            final Writer writer,
            final String hostCharset) throws HostTransformException;

    /**
     * Transforms host data to XML with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final String hostCharset) throws HostTransformException;

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    void transform(
            final byte[] hostData,
            final Writer writer) throws HostTransformException;

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer) throws HostTransformException;

    /**
     * Transforms host data to XML with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    void transform(
            final byte[] hostData,
            final Writer writer,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms host data to XML with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    void transform(
            final byte[] hostData,
            final Writer writer,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer XML will be sent to this writer.
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final HostTransformStatus status) throws HostTransformException;
}
