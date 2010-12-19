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
package com.legstar.mq.mqcih.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractXmlToHostTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms XML to mainframe data.
 * <p/>
 * This is a typical use of this class:
 * 
 * <pre>
 * StringReader reader = new StringReader(&quot;&lt;someXml&gt;...&lt;/someXml&gt;&quot;);
 * MqcihXmlToHostTransformer transformer = new MqcihXmlToHostTransformer();
 * byte[] hostByteArray = transformer.transform(new StreamSource(reader));
 * </pre>
 * 
 */
public class MqcihXmlToHostTransformer extends AbstractXmlToHostTransformer {

    /**
     * Create a XML to Host transformer using a Java to Host transformer.
     * 
     * @throws HostTransformException if transformer cannot be created
     */
    public MqcihXmlToHostTransformer() throws HostTransformException {
        super(new MqcihJavaToHostTransformer());
    }

    /**
     * Create an XML to Host transformer using a specific COBOL parameters set.
     * 
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public MqcihXmlToHostTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new MqcihJavaToHostTransformer(cobolContext));
    }

    /**
     * Create an XML to Host transformer using a specific host character set
     * while
     * other COBOL parameters are set by default.
     * 
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public MqcihXmlToHostTransformer(
            final String hostCharset) throws HostTransformException {
        super(new MqcihJavaToHostTransformer(hostCharset));
    }

}
