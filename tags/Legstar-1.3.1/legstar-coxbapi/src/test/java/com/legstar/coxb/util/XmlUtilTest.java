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
    
    /**
     * Demonstrates Issue 47.
     */
    public void testDrainage() {
        StringReader reader = new StringReader(
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
                + "<Dfhcommarea xmlns=\"http://legstar.com/test/coxb/lsfileae\">"
                + "<ComNumber>100</ComNumber>"
                + "<ComPersonal>"
                + "<ComName>TOTO</ComName>"
                + "<ComAddress>LABAS STREET</ComAddress>"
                + "<ComPhone>88993314</ComPhone>"
                + "</ComPersonal>"
                + "<ComDate>100458</ComDate>"
                + "<ComAmount>00100.35</ComAmount>"
                + "<ComComment>A VOIR</ComComment>"
                + "</Dfhcommarea>");
        Source source = new StreamSource(reader);
        String result = XmlUtil.prettyPrint(source);
        assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
                + "<Dfhcommarea xmlns=\"http://legstar.com/test/coxb/lsfileae\">\r\n"
                + "  <ComNumber>100</ComNumber>\r\n"
                + "  <ComPersonal>\r\n"
                + "    <ComName>TOTO</ComName>\r\n"
                + "    <ComAddress>LABAS STREET</ComAddress>\r\n"
                + "    <ComPhone>88993314</ComPhone>\r\n"
                + "  </ComPersonal>\r\n"
                + "  <ComDate>100458</ComDate>\r\n"
                + "  <ComAmount>00100.35</ComAmount>\r\n"
                + "  <ComComment>A VOIR</ComComment>\r\n"
                + "</Dfhcommarea>\r\n", result);
        result = XmlUtil.prettyPrint(source);
        assertTrue(result.equals("java.io.IOException: Stream closed"));
    }

}
