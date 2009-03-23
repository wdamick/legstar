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

import javax.xml.transform.Source;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.util.XmlUtil;

/**
 * Generic methods to transform XML to host data.
 */
public abstract class AbstractXmlToHostTransformer implements IXmlToHostTransformer {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(AbstractXmlToHostTransformer.class);
    
    /** A Java object to Host transformer. */
    private IJavaToHostTransformer mJavaToHostTransformer;

    /**
     * Create an XML to Host transformer using a Java to Host transformer.
     * @param javaToHostTransformer the java to host transformer
     */
    public AbstractXmlToHostTransformer(final IJavaToHostTransformer javaToHostTransformer) {
        mJavaToHostTransformer = javaToHostTransformer;
    }

    /**
     * Transforms XML to host data with a specific host character set.
     * @param source the XML Source to unmarshal XML data from (such as SAXSource, DOMSource, and StreamSource)
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Source source, final String hostCharset) throws HostTransformException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Transforming XML to host data:");
            LOG.debug(XmlUtil.prettyPrint(source));
        }
        return getJavaToHostTransformer().transform(getObjectFromXml(source), hostCharset);
    }

    /**
     * Transforms XML to host data.
     * @param source the XML Source to unmarshal XML data from (such as SAXSource, DOMSource, and StreamSource)
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Source source) throws HostTransformException {
        return transform(source, null);
    }
    
    /**
     * Unmarshal an XML to get the JAXB value object.
     * @param source the XML Source to unmarshal XML data from (such as SAXSource, DOMSource, and StreamSource)
     * @return a JAXB value object
     * @throws HostTransformException if transformation fails
     */
    public abstract Object getObjectFromXml(final Source source) throws HostTransformException;

    /**
     * @return the Java to Host transformer
     */
    public IJavaToHostTransformer getJavaToHostTransformer() {
        return mJavaToHostTransformer;
    }

 }
