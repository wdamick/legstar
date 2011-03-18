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

import java.io.StringReader;

import javax.xml.transform.stream.StreamSource;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.CharsetsCases;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.MSNSearchCases;

import junit.framework.TestCase;

/**
 * Test ReflectXmlToHostTransformer class.
 *
 */
public class ReflectXmlToHostTransformerTest extends TestCase {

    /**
     * Create host data from XML with a non root element.
     */
    public void testNonRootElement() {
        try {
            ReflectXmlToHostTransformer transformer =
                new ReflectXmlToHostTransformer("com.legstar.test.coxb.lsfileae", "Dfhcommarea");
            StringReader reader = new StringReader(LsfileaeCases.getXml());
            assertEquals(LsfileaeCases.getHostBytesHex(),
                    HostData.toHexString(transformer.transform(new StreamSource(reader))));
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
        
    }

    /**
     * Create an XML with a special host character set.
     */
    public void testHostCharset() {
        try {
            
            ReflectXmlToHostTransformer transformer =
                new ReflectXmlToHostTransformer("com.legstar.test.coxb.charsets", "Dfhcommarea");
            StringReader reader = new StringReader(CharsetsCases.getXml());
            assertEquals(CharsetsCases.getHostBytesHexIBM01140(),
                    HostData.toHexString(transformer.transform(new StreamSource(reader))));
            reader = new StringReader(CharsetsCases.getXml());
            assertEquals(CharsetsCases.getHostBytesHex(),
                    HostData.toHexString(transformer.transform(new StreamSource(reader), "IBM01147")));
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Create host data from XML with a root element.
     */
    public void testRootElement() {
        try {
            ReflectXmlToHostTransformer transformer =
                new ReflectXmlToHostTransformer("com.legstar.test.coxb.MSNSearch", "Search");
            StringReader reader = new StringReader(MSNSearchCases.getXml());
            assertEquals(MSNSearchCases.getHostBytesHexRequest(),
                    HostData.toHexString(transformer.transform(new StreamSource(reader))));
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
        
    }
}
