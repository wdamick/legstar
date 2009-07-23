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
package com.legstar.eclipse.plugin.schemagen.util;

import java.io.IOException;
import java.io.Writer;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * XML sources can be XML Schema or WSDL. This class provides loading
 * and pretty printing capabilities for XML in general.
 * TODO evolve this by inferring the source type (either XML schema or
 * WSDL) end formatting accordingly. We could extract the type node
 * from WSDL so that all output is XML Schema whatever the source.
 */
public class XmlDocumentHelper {

    /** Reusable parser. */
    private DocumentBuilder mDocumentBuilder;

    /** The XML document. */
    private Document mDocument;

    /**
     * Load the source as a DOM document.
     * @param sourceName the source name should be a valid URI
     * @throws XmlDocumentHelperException if load fails
     */
    public void load(
            final String sourceName) throws XmlDocumentHelperException {
        lazyInit();
        try {
            mDocument = mDocumentBuilder.parse(sourceName);
        } catch (SAXException e) {
            throw new XmlDocumentHelperException(e);
        } catch (IOException e) {
            throw new XmlDocumentHelperException(e);
        }
    }

    /**
     * Serialize the document with some indentation so that it is readable.
     * @param out an output writer
     * @throws XmlDocumentHelperException if serializing fails
     */
    public void serialize(
            final Writer out) throws XmlDocumentHelperException {

        if (mDocument == null) {
            throw new XmlDocumentHelperException("No XML was loaded");
        }
        TransformerFactory tfactory = TransformerFactory.newInstance();
        Transformer serializer;
        try {
            serializer = tfactory.newTransformer();
            //Setup indenting to "pretty print"
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.setOutputProperty(
                    "{http://xml.apache.org/xslt}indent-amount", "2");

            serializer.transform(
                    new DOMSource(mDocument), new StreamResult(out));
        } catch (TransformerException e) {
            throw new XmlDocumentHelperException(e);
        }
    }

    /**
     * On first call this creates an instance of the XML parser. This allows
     * loading to affect the first call only.
     * @throws XmlDocumentHelperException if oarser initialization fails
     */
    private void lazyInit() throws XmlDocumentHelperException {
        if (mDocumentBuilder == null) {
            try {
                DocumentBuilderFactory factory =
                    DocumentBuilderFactory.newInstance();
                mDocumentBuilder = factory.newDocumentBuilder();
            } catch (ParserConfigurationException e) {
                throw new XmlDocumentHelperException(e);
            }
        }
    }
}
