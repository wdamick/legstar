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

import java.io.StringReader;
import java.io.Writer;

import javax.xml.transform.stream.StreamSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.util.XmlUtil;

/**
 * Generic methods to transform host data to XML.
 */
public abstract class AbstractHostToXmlTransformer implements IHostToXmlTransformer {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(AbstractHostToXmlTransformer.class);
    
    /** A Host to Java object transformer. */
    private IHostToJavaTransformer mHostToJavaTransformer;

    /**
     * Create an Host to XML transformer using a Host to Java transformer.
     * @param hostToJavaTransformer the host to java transformer
     */
    public AbstractHostToXmlTransformer(final IHostToJavaTransformer hostToJavaTransformer) {
        mHostToJavaTransformer = hostToJavaTransformer;
    }

    /**
     * Transforms host data to XML with a specific host character set.
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData 
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    public void  transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final String hostCharset) throws HostTransformException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Transforming host data to XML:");
        }
        Object valueObject = getHostToJavaTransformer().transform(hostData, offset, hostCharset);
        getXmlFromObject(valueObject, writer);
        if (LOG.isDebugEnabled()) {
            StringReader reader = new StringReader(writer.toString());
            LOG.debug(XmlUtil.prettyPrint(new StreamSource(reader)));
        }
    }

    /**
     * Transforms host data to XML with a specific host character set.
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final Writer writer,
            final String hostCharset) throws HostTransformException {
        transform(hostData, 0, writer, hostCharset);
    }
    /**
     * Transforms host data to XML.
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final Writer writer) throws HostTransformException {
        transform(hostData, 0, writer, null);
    }
    
    /**
     * Transforms host data to XML.
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData 
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer) throws HostTransformException {
        transform(hostData, offset, writer, null);
    }

    /**
     * Marshal JAXB value object  to get the XML.
     * @param valueObject the JAXB value object
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public abstract void getXmlFromObject(
            final Object valueObject, final Writer writer) throws HostTransformException;

    /**
     * @return the Host to Java transformer
     */
    public IHostToJavaTransformer getHostToJavaTransformer() {
        return mHostToJavaTransformer;
    }

 }
