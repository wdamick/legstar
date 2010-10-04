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

import java.io.Writer;

import javax.xml.transform.Source;

/**
 * Alternative to {@link IHostTransformers} which works on XML rather than java
 * object.
 * <p/>
 * Classes implementing this interface can transform XML to mainframe data
 * streams and vice versa.
 * 
 */
public interface IHostXmlTransformers {

    /**
     * Transforms XML source to host data with a specific host character set.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(
            final Source source, final String hostCharset)
            throws HostTransformException;

    /**
     * Transforms XML source to host data.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(final Source source) throws HostTransformException;

    /**
     * Transforms host data to XML using a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    void toXml(final byte[] hostData, final Writer writer,
            final String hostCharset)
            throws HostTransformException;

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    void toXml(final byte[] hostData, final Writer writer)
            throws HostTransformException;

    /**
     * Transforms XML source to host data with a specific host character set.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(
            final Source source, final String hostCharset,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms XML source to host data.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] toHost(final Source source,
            final HostTransformStatus status) throws HostTransformException;

    /**
     * Transforms host data to XML using a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    void toXml(final byte[] hostData, final Writer writer,
            final String hostCharset,
            final HostTransformStatus status)
            throws HostTransformException;

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    void toXml(final byte[] hostData, final Writer writer,
            final HostTransformStatus status) throws HostTransformException;

}
