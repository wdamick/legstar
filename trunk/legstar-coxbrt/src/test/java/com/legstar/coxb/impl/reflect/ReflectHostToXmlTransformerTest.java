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
package com.legstar.coxb.impl.reflect;

import java.io.StringWriter;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.CharsetsCases;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.MSNSearchCases;

import junit.framework.TestCase;

/**
 * Test ReflectHostToXmlTransformer class.
 *
 */
public class ReflectHostToXmlTransformerTest extends TestCase {
    
    /**
     * Create an XML from host data with a non root element.
     */
    public void testNonRootElement() {
        try {
            
            ReflectHostToXmlTransformer transformer =
                new ReflectHostToXmlTransformer("com.legstar.test.coxb.lsfileae", "Dfhcommarea");
            StringWriter writer = new StringWriter();
            transformer.transform(HostData.toByteArray(LsfileaeCases.getHostBytesHex()), writer);
            assertEquals(LsfileaeCases.getXml(), writer.toString());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Create an XML from host data at a certain offset.
     */
    public void testHostDataOffset() {
        try {
            
            ReflectHostToXmlTransformer transformer =
                new ReflectHostToXmlTransformer("com.legstar.test.coxb.lsfileae", "ComPersonal");
            StringWriter writer = new StringWriter();
            transformer.transform(HostData.toByteArray(LsfileaeCases.getHostBytesHex()), 6, writer);
            assertEquals(LsfileaeCases.getComPersonalXml(), writer.toString());
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
            
            ReflectHostToXmlTransformer transformer =
                new ReflectHostToXmlTransformer("com.legstar.test.coxb.charsets", "Dfhcommarea");
            StringWriter writer = new StringWriter();
            transformer.transform(HostData.toByteArray(CharsetsCases.getHostBytesHex()), writer);
            assertEquals(CharsetsCases.getXmlIBM01140(), writer.toString());
            writer = new StringWriter();
            transformer.transform(HostData.toByteArray(CharsetsCases.getHostBytesHex()), writer, "IBM01147");
            assertEquals(CharsetsCases.getXml(), writer.toString());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Create an XML from host data with a root element.
     */
    public void testRootElement() {
        try {
            
            ReflectHostToXmlTransformer transformer =
                new ReflectHostToXmlTransformer("com.legstar.test.coxb.MSNSearch", "Search");
            StringWriter writer = new StringWriter();
            transformer.transform(HostData.toByteArray(MSNSearchCases.getHostBytesHexRequest()), writer);
            assertEquals(MSNSearchCases.getXml(), writer.toString());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}
