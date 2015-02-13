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

import java.io.Writer;

import javax.xml.transform.Source;

/**
 * A generic class that provides transformer capabilities for a given structure.
 * <p/>
 * A structure maps to an XSD and a COBOL structure. This class does not
 * implement the transformers, it acts as a container.
 * <p/>
 * Classes derived from this one will typically implement a constructor that
 * creates the directional transformers, XML to host and host to XML.
 * 
 */
public abstract class AbstractXmlTransformers implements IHostXmlTransformers {

    /** Transformer that turns an XML into host data. */
    private IXmlToHostTransformer mXmlToHost;

    /** Transformer that turns host data into an XML. */
    private IHostToXmlTransformer mHostToXml;

    /**
     * No arg constructor. Caller is responsible for setting the internal
     * transformers.
     */
    public AbstractXmlTransformers() {

    }

    /**
     * Creates a provider with its directional transformers.
     * 
     * @param xmlToHost XML to host transformer
     * @param hostToXml host to XML transformer
     */
    public AbstractXmlTransformers(
            final IXmlToHostTransformer xmlToHost,
            final IHostToXmlTransformer hostToXml) {
        mXmlToHost = xmlToHost;
        mHostToXml = hostToXml;
    }

    /**
     * @return the transformer that turns a XML into host data
     */
    public IXmlToHostTransformer getXmlToHost() {
        return mXmlToHost;
    }

    /**
     * @param xmlToHost the transformer that turns an XML into host data to set
     */
    public void setXmlToHost(final IXmlToHostTransformer xmlToHost) {
        mXmlToHost = xmlToHost;
    }

    /**
     * @return the transformer that turns host data into an XML
     */
    public IHostToXmlTransformer getHostToXml() {
        return mHostToXml;
    }

    /**
     * @param hostToXml the transformer that turns host data into an XML to set
     */
    public void setHostToXml(final IHostToXmlTransformer hostToXml) {
        mHostToXml = hostToXml;
    }

    /**
     * Transforms XML to host data with a specific host character set.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Source source, final String hostCharset)
            throws HostTransformException {
        return getXmlToHost().transform(source, hostCharset);
    }

    /**
     * Transforms XML to host data.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Source source)
            throws HostTransformException {
        return getXmlToHost().transform(source);
    }

    /**
     * Transforms host data to XML with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    public void toXml(final byte[] hostData, final Writer writer,
            final String hostCharset)
            throws HostTransformException {
        getHostToXml().transform(hostData, writer, hostCharset);
    }

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public void toXml(final byte[] hostData, final Writer writer)
            throws HostTransformException {
        getHostToXml().transform(hostData, writer);
    }

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
    public byte[] toHost(final Source source, final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        return getXmlToHost().transform(source, hostCharset, status);
    }

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
    public byte[] toHost(final Source source, final HostTransformStatus status)
            throws HostTransformException {
        return getXmlToHost().transform(source, status);
    }

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
    public void toXml(final byte[] hostData, final Writer writer,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        getHostToXml().transform(hostData, writer, hostCharset, status);
    }

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    public void toXml(final byte[] hostData, final Writer writer,
            final HostTransformStatus status)
            throws HostTransformException {
        getHostToXml().transform(hostData, writer, status);
    }
}
