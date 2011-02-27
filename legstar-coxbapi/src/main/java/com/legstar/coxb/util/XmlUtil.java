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
package com.legstar.coxb.util;

import java.io.StringWriter;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;

/**
 * A utility class for XML manipulation.
 *
 */
public final class XmlUtil {
    
    /**
     * Utility class.
     */
    private XmlUtil() {
        
    }
    
    /**
     * Pretty print an XML. Use with caution as this drains the source if it is
     * a StreamSource.
     * @param source the XML source
     * @return a String with readable XML
     */
    public static String prettyPrint(final Source source) {
        try {
            Transformer serializer = TransformerFactory.newInstance().newTransformer();
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            StringWriter writer = new StringWriter();
            serializer.transform(source, new StreamResult(writer));
            return writer.toString();
        } catch (TransformerException e) {
            return e.getMessage();
        }
    }

}
