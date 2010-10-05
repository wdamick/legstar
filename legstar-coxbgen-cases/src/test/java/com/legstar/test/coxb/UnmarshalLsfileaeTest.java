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
package com.legstar.test.coxb;

import java.io.StringWriter;

import junit.framework.TestCase;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.coxb.transform.HostTransformStatus;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaHostToJavaTransformer;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaJsonTransformers;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTransformers;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaXmlTransformers;

/**
 * Unmarshal lsfileae.
 * 
 */
public class UnmarshalLsfileaeTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * 
     * @throws Exception if marshaling fails
     */
    public void testLsfileae() throws Exception {

        String hexString = LsfileaeCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes,
                "lsfileae");
        LsfileaeCases.checkJavaObject(dfhcommarea);
    }

    /**
     * Transform host data and test java data object result.
     * 
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformer() throws HostTransformException {

        HostTransformStatus status = new HostTransformStatus();
        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData
                .toByteArray(LsfileaeCases.getHostBytesHex()), status);
        LsfileaeCases.checkJavaObject(dfhcommarea);
        assertEquals(79, status.getHostBytesProcessed());
    }

    /**
     * Test the sample code shown in documentation.
     * 
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformerDoc() throws HostTransformException {

        hostToJavaTransform(HostData.toByteArray(LsfileaeCases
                .getHostBytesHex()));
        hostToXmlTransform(HostData
                .toByteArray(LsfileaeCases.getHostBytesHex()));
        hostToJsonTransform(HostData
                .toByteArray(LsfileaeCases.getHostBytesHex()));
    }

    /**
     * Transform host data and test java data object result.
     * 
     * @param hostBytes a byte array holding the mainframe payload
     * @throws HostTransformException if transforming fails
     */
    public void hostToJavaTransform(final byte[] hostBytes)
            throws HostTransformException {

        DfhcommareaTransformers transformers = new DfhcommareaTransformers();
        Dfhcommarea dfhcommarea = transformers.toJava(hostBytes);
        assertEquals(100, dfhcommarea.getComNumber());
        assertEquals("TOTO", dfhcommarea.getComPersonal().getComName().trim());
        assertEquals("LABAS STREET", dfhcommarea.getComPersonal()
                .getComAddress().trim());
        assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone()
                .trim());
        assertEquals("100458", dfhcommarea.getComDate().trim());
        assertEquals("00100.35", dfhcommarea.getComAmount().trim());
        assertEquals("A VOIR", dfhcommarea.getComComment().trim());
    }

    /**
     * Transform host data and test XML result.
     * 
     * @param hostBytes a byte array holding the mainframe payload
     * @throws HostTransformException if transforming fails
     */
    public void hostToXmlTransform(final byte[] hostBytes)
            throws HostTransformException {

        DfhcommareaXmlTransformers transformers =
                new DfhcommareaXmlTransformers();
        StringWriter writer = new StringWriter();
        transformers.toXml(hostBytes, writer);
        assertEquals(
                "<?xml version=\"1.0\" encoding=\"UTF-8\" "
                        + "standalone=\"yes\"?>"
                        + "<Dfhcommarea xmlns="
                        + "\"http://legstar.com/test/coxb/lsfileae\">"
                        + "<ComNumber>100</ComNumber>"
                        + "<ComPersonal>"
                        + "<ComName>TOTO</ComName>"
                        + "<ComAddress>LABAS STREET</ComAddress>"
                        + "<ComPhone>88993314</ComPhone>"
                        + "</ComPersonal>"
                        + "<ComDate>100458</ComDate>"
                        + "<ComAmount>00100.35</ComAmount>"
                        + "<ComComment>A VOIR</ComComment>"
                        + "</Dfhcommarea>", writer.toString());
    }

    /**
     * Transform host data and test JSON result.
     * 
     * @param hostBytes a byte array holding the mainframe payload
     * @throws HostTransformException if transforming fails
     */
    public void hostToJsonTransform(final byte[] hostBytes)
            throws HostTransformException {

        DfhcommareaJsonTransformers transformers =
                new DfhcommareaJsonTransformers();
        StringWriter writer = new StringWriter();
        transformers.toJson(hostBytes, writer);
        assertEquals("{\"ComNumber\":100,"
                + "\"ComPersonal\":"
                + "{\"ComName\":\"TOTO\","
                + "\"ComAddress\":\"LABAS STREET\","
                + "\"ComPhone\":\"88993314\"},"
                + "\"ComDate\":\"100458\","
                + "\"ComAmount\":\"00100.35\","
                + "\"ComComment\":\"A VOIR\"}",
                writer.toString());
    }

    /**
     * Transform host data and test JSON result.
     * 
     * @param hostBytes a byte array holding the mainframe payload
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJsonTransform() throws HostTransformException {

        DfhcommareaJsonTransformers transformers = new DfhcommareaJsonTransformers();
        StringWriter writer = new StringWriter();
        transformers.toJson(HostData
                .toByteArray(LsfileaeCases.getHostBytesHex()), writer);
        assertEquals(LsfileaeCases.getJson(), writer.toString());
    }
}
