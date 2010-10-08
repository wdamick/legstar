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

import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.transform.stream.StreamSource;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.CharsetsCases;
import com.legstar.test.coxb.LsfileaeCases;

import junit.framework.TestCase;

/**
 * Test ReflectXmlTransformers class.
 */
public class ReflectXmlTransformersTest extends TestCase {
    
    /**
     * LSFILAE case.
     */
    public void testLsfileae() {
        try {
            ReflectXmlTransformers transformers =
                new ReflectXmlTransformers("com.legstar.test.coxb.lsfileae", "Dfhcommarea");
            StringReader reader = new StringReader(LsfileaeCases.getXml());
            assertEquals(LsfileaeCases.getHostBytesHex(),
                    HostData.toHexString(transformers.toHost(new StreamSource(reader))));
            StringWriter writer = new StringWriter();
            transformers.toXml(HostData.toByteArray(LsfileaeCases.getHostBytesHex()),
                    writer);
            assertEquals(LsfileaeCases.getXml(), writer.toString());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * CHARSETS case.
     */
    public void testCharsets() {
        try {
            ReflectXmlTransformers transformers =
                new ReflectXmlTransformers("com.legstar.test.coxb.charsets", "Dfhcommarea");
            StringReader reader = new StringReader(CharsetsCases.getXml());
            assertEquals(CharsetsCases.getHostBytesHex(),
                    HostData.toHexString(transformers.toHost(new StreamSource(reader), "IBM01147")));
            StringWriter writer = new StringWriter();
            transformers.toXml(HostData.toByteArray(CharsetsCases.getHostBytesHex()),
                    writer, "IBM01147");
            assertEquals(CharsetsCases.getXml(), writer.toString());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}
