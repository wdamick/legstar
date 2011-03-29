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

import java.io.Writer;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolBindingException;

/**
 * Generic methods to transform host data to XML.
 */
public abstract class AbstractHostToXmlTransformer implements
        IHostToXmlTransformer {

    /** Logger. */
    private final Log _log = LogFactory
            .getLog(AbstractHostToXmlTransformer.class);

    /** A Host to Java object transformer. */
    private IHostToJavaTransformer mHostToJavaTransformer;

    /** JAXB Context. */
    private JAXBContext mJaxbContext = null;

    /** JAXB Marshaller (Object to XML). */
    private Marshaller mXmlMarshaller = null;

    /**
     * Create an Host to XML transformer using a Host to Java transformer.
     * 
     * @param hostToJavaTransformer the host to java transformer
     * @throws HostTransformException if transformer cannot be created
     */
    public AbstractHostToXmlTransformer(
            final IHostToJavaTransformer hostToJavaTransformer)
            throws HostTransformException {
        try {
            mHostToJavaTransformer = hostToJavaTransformer;
            mJaxbContext = JAXBContext.newInstance(
                    mHostToJavaTransformer.newBinding().getJaxbType());
            mXmlMarshaller = mJaxbContext.createMarshaller();
        } catch (JAXBException e) {
            throw new HostTransformException(e);
        } catch (CobolBindingException e) {
            throw new HostTransformException(e);
        }
    }

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
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        if (_log.isDebugEnabled()) {
            _log.debug("Transforming host data to XML:");
        }
        Object valueObject = getHostToJavaTransformer().transform(hostData,
                offset, hostCharset, status);
        getXmlFromObject(valueObject, writer);
    }

    /**
     * Transforms host data to XML with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer XML will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final String hostCharset) throws HostTransformException {
        transform(hostData, offset, writer, hostCharset,
                new HostTransformStatus());
    }

    /**
     * Transforms host data to XML with a specific host character set.
     * 
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
     * 
     * @param hostData a byte array containing host data
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final Writer writer) throws HostTransformException {
        transform(hostData, 0, writer, (String) null);
    }

    /**
     * Transforms host data to XML.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer) throws HostTransformException {
        transform(hostData, offset, writer, (String) null);
    }

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
    public void transform(
            final byte[] hostData,
            final Writer writer,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        transform(hostData, 0, writer, hostCharset, status);
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
    public void transform(
            final byte[] hostData,
            final Writer writer,
            final HostTransformStatus status) throws HostTransformException {
        transform(hostData, 0, writer, (String) null, status);
    }

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
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final HostTransformStatus status) throws HostTransformException {
        transform(hostData, offset, writer, (String) null, status);
    }

    /**
     * Marshal JAXB value object to get the XML.
     * <p/>
     * Root elements can be marshalled directly while non-root elements must be
     * encapsulated in a JAXBElement before they can be marshalled.
     * 
     * @param valueObject the JAXB value object
     * @param writer XML will be sent to this writer.
     * @throws HostTransformException if transformation fails
     * */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public void getXmlFromObject(
            final Object valueObject, final Writer writer)
            throws HostTransformException {
        try {
            if (isXmlRootElement()) {
                getXmlMarshaller().marshal(valueObject, writer);
            } else {
                QName qName = new QName(
                        getNamespace(),
                        getElementName());
                JAXBElement < ? > jaxbElement = new JAXBElement(qName,
                            getHostToJavaTransformer().newBinding()
                                    .getJaxbType(),
                            valueObject);
                getXmlMarshaller().marshal(jaxbElement, writer);
            }
        } catch (JAXBException e) {
            throw new HostTransformException(e);
        } catch (CobolBindingException e) {
            throw new HostTransformException(e);
        }
    }

    /**
     * @return true if the JAXB element is marked as XmlRootElement which
     *         means it does not need to be encapsulated in a JAXBElement.
     */
    public abstract boolean isXmlRootElement();

    /**
     * @return the XML Schema namespace
     */
    public abstract String getNamespace();

    /**
     * @return the element name is given either by the XmlRootElement
     *         annotation or the XmlType annotation.
     */
    public abstract String getElementName();

    /**
     * @return the Host to Java transformer
     */
    public IHostToJavaTransformer getHostToJavaTransformer() {
        return mHostToJavaTransformer;
    }

    /**
     * @return the JAXB Marshaller (Object to XML)
     */
    public JAXBContext getJAXBContext() {
        return mJaxbContext;
    }

    /**
     * @return the JAXB Marshaller (Object to XML)
     */
    public Marshaller getXmlMarshaller() {
        return mXmlMarshaller;
    }

}
