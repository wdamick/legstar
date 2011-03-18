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
package com.legstar.coxb.impl.reflect;

import java.io.Writer;

import javax.xml.transform.Source;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Groups a couple of transformers for dynamic XML to Host and
 * Host to XML transformations.
 *
 */
public class ReflectXmlTransformers extends AbstractXmlTransformers {

    /**
     * Construct a transformer for a particular JAXB type.
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws ReflectBindingException 
     * @throws ReflectBindingException if JAXB type has no annotations or cannot be 
     *  located from the classpath
     * @throws HostTransformException if transformer cannot be created
     */
    public ReflectXmlTransformers(
            final String jaxbPackageName,
            final String jaxbType) throws ReflectBindingException, HostTransformException {
        super(new ReflectXmlToHostTransformer(jaxbPackageName, jaxbType),
                new ReflectHostToXmlTransformer(jaxbPackageName, jaxbType));
    }

    /** {@inheritDoc} */
    public byte[] toHost(final Source source, final String hostCharset)
            throws HostTransformException {
        return getXmlToHost().transform(source, hostCharset);
    }

    /** {@inheritDoc} */
    public byte[] toHost(final Source source) throws HostTransformException {
        return getXmlToHost().transform(source);
    }

    /** {@inheritDoc} */
    public void toXml(final byte[] hostData, final Writer writer, final String hostCharset)
            throws HostTransformException {
        getHostToXml().transform(hostData, writer, hostCharset);
    }

    /** {@inheritDoc} */
    public void toXml(final byte[] hostData, final Writer writer)
            throws HostTransformException {
        getHostToXml().transform(hostData, writer);
    }


}
