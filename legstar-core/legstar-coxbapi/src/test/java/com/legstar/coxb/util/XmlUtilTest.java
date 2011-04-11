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
package com.legstar.coxb.util;

import java.io.StringReader;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import junit.framework.TestCase;

/**
 * Test XmlUtil class.
 * 
 */
public class XmlUtilTest extends TestCase {

    /** Platform specific line separator. */
    private static final String LINE_SEPARATOR = System
            .getProperty("line.separator");

    /**
     * Demonstrates Issue 47.
     */
    public void testDrainage() {
        StringReader reader = new StringReader(
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
                        + "<Dfhcommarea xmlns=\"http://legstar.com/test/coxb/lsfileae\">"
                        + "<ComNumber>100</ComNumber>" + "<ComPersonal>"
                        + "<ComName>TOTO</ComName>"
                        + "<ComAddress>LABAS STREET</ComAddress>"
                        + "<ComPhone>88993314</ComPhone>" + "</ComPersonal>"
                        + "<ComDate>100458</ComDate>"
                        + "<ComAmount>00100.35</ComAmount>"
                        + "<ComComment>A VOIR</ComComment>" + "</Dfhcommarea>");
        Source source = new StreamSource(reader);
        String result = XmlUtil.prettyPrint(source);
        assertEquals(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                        + LINE_SEPARATOR
                        + "<Dfhcommarea xmlns=\"http://legstar.com/test/coxb/lsfileae\">"
                        + LINE_SEPARATOR + "  <ComNumber>100</ComNumber>"
                        + LINE_SEPARATOR + "  <ComPersonal>" + LINE_SEPARATOR
                        + "    <ComName>TOTO</ComName>" + LINE_SEPARATOR
                        + "    <ComAddress>LABAS STREET</ComAddress>"
                        + LINE_SEPARATOR + "    <ComPhone>88993314</ComPhone>"
                        + LINE_SEPARATOR + "  </ComPersonal>" + LINE_SEPARATOR
                        + "  <ComDate>100458</ComDate>" + LINE_SEPARATOR
                        + "  <ComAmount>00100.35</ComAmount>" + LINE_SEPARATOR
                        + "  <ComComment>A VOIR</ComComment>" + LINE_SEPARATOR
                        + "</Dfhcommarea>" + LINE_SEPARATOR, result);
        result = XmlUtil.prettyPrint(source);
        assertTrue(result.equals("java.io.IOException: Stream closed"));
    }

}
