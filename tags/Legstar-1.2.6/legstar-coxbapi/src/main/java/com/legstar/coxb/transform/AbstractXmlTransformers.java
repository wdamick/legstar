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


/**
 * A generic class that provides transformer capabilities for a given structure.
 * <p/>
 * A structure maps to an XSD and a COBOL structure.
 * This class does not implement the transformers, it acts as a container.
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
     * No arg constructor. Caller is responsible for setting the internal transformers.
     */
    public AbstractXmlTransformers() {
        
    }
    
    /**
     * Creates a provider with its directional transformers.
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

}
