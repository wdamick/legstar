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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolBindingException;

/**
 * Generic methods to transform XML to host data.
 */
public abstract class AbstractXmlToHostTransformer implements
        IXmlToHostTransformer {

    /** Logger. */
    private final Log _log = LogFactory
            .getLog(AbstractXmlToHostTransformer.class);

    /** A Java object to Host transformer. */
    private IJavaToHostTransformer mJavaToHostTransformer;

    /** JAXB Context. */
    private JAXBContext mJaxbContext = null;

    /** JAXB Unmarshaller (XML to Object). */
    private Unmarshaller mXmlUnmarshaller = null;

    /**
     * Create an XML to Host transformer using a Java to Host transformer.
     * 
     * @param javaToHostTransformer the java to host transformer
     * @throws HostTransformException if transformer cannot be created
     */
    public AbstractXmlToHostTransformer(
            final IJavaToHostTransformer javaToHostTransformer)
            throws HostTransformException {
        try {
            mJavaToHostTransformer = javaToHostTransformer;
            mJaxbContext = JAXBContext.newInstance(
                    mJavaToHostTransformer.getBinding().getJaxbType());
            mXmlUnmarshaller = mJaxbContext.createUnmarshaller();
        } catch (JAXBException e) {
            throw new HostTransformException(e);
        } catch (CobolBindingException e) {
            throw new HostTransformException(e);
        }
    }

    /**
     * Transforms XML to host data with a specific host character set.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Source source, final String hostCharset,
            final HostTransformStatus status)
            throws HostTransformException {
        if (_log.isDebugEnabled()) {
            _log.debug("Transforming XML to host data:");
        }
        return getJavaToHostTransformer().transform(getObjectFromXml(source),
                hostCharset, status);
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
    public byte[] transform(final Source source, final String hostCharset)
            throws HostTransformException {
        return transform(source, hostCharset, new HostTransformStatus());
    }

    /**
     * Transforms XML to host data.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Source source) throws HostTransformException {
        return transform(source, (String) null);
    }

    /**
     * Transforms XML to host data.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Source source,
            final HostTransformStatus status)
            throws HostTransformException {
        return transform(source, (String) null, status);
    }

    /**
     * Unmarshal an XML to get the JAXB value object.
     * 
     * @param source the XML Source to unmarshal XML data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @return a JAXB value object
     * @throws HostTransformException if transformation fails
     */
    public Object getObjectFromXml(final Source source)
            throws HostTransformException {
        try {
            return getXmlUnmarshaller().unmarshal(source,
                    getJavaToHostTransformer().getBinding().getJaxbType())
                    .getValue();
        } catch (JAXBException e) {
            throw new HostTransformException(e);
        } catch (CobolBindingException e) {
            throw new HostTransformException(e);
        }
    }

    /**
     * @return the Java to Host transformer
     */
    public IJavaToHostTransformer getJavaToHostTransformer() {
        return mJavaToHostTransformer;
    }

    /**
     * @return the JAXB Unmarshaller (Object to XML)
     */
    public Unmarshaller getXmlUnmarshaller() {
        return mXmlUnmarshaller;
    }
}
