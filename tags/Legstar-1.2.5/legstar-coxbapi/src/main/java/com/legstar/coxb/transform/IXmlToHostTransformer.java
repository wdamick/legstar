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

/**
 * XML to Host transformers offer the capability to turn an XML to raw mainframe byte arrays.
 *
 */
public interface IXmlToHostTransformer {

    /**
     * Transforms XML to host data with a specific host character set.
     * @param source the XML Source to unmarshal XML data from (such as SAXSource, DOMSource, and StreamSource)
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Source source, final String hostCharset) throws HostTransformException;

    /**
     * Transforms XML to host data.
     * @param source the XML Source to unmarshal XML data from (such as SAXSource, DOMSource, and StreamSource)
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Source source) throws HostTransformException;

}
