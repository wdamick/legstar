/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.cixs.gen.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.TransformerException;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.sax.SAXSource;
import org.xml.sax.XMLReader;
import org.xml.sax.InputSource;


import org.xml.sax.SAXException;

import junit.framework.TestCase;

public class XIncludeTest extends TestCase {
	
	public String parsedData;
	
	public void testInclude() throws ParserConfigurationException, SAXException, IOException {
		/** Forcing XML parser to resolve XInclude */
		SAXParserFactory spf = SAXParserFactory.newInstance();
		spf.setXIncludeAware(true);
		spf.setNamespaceAware(true);
		javax.xml.parsers.SAXParser saxParser = spf.newSAXParser();
		parsedData = "";
		saxParser.parse(new File("./src/xml-models/cixs-sd-lsfileae.xml"), new MyHandler());
		assertEquals("cixs-serviceservice-nameservice-endpoint-packageservice-targetnamespacecixs-operationoperation-nameprogram-nameinputoutput", parsedData);
	}
	
	public void testXSLTC() throws SAXException, ParserConfigurationException, TransformerException, IOException  {
		
        File tmpFile = File.createTempFile("SEI", "java");

		StreamResult  result = new StreamResult(tmpFile);
  		StreamSource  xsltSrce = new StreamSource("./src/main/resources/xslt/sun-jaxws-xml.xsl");

		/** Force the SAX parser to resolve XInclude in the source XML*/
		SAXParserFactory spf = SAXParserFactory.newInstance();
		spf.setXIncludeAware(true);
		spf.setNamespaceAware(true);
		XMLReader reader= spf.newSAXParser().getXMLReader();
		SAXSource xmlSrce= new SAXSource(reader, new InputSource("./src/xml-models/cixs-sd-lsfileae.xml"));

		/** Make sure we have an XSLT 2.0 transformerFactory in the classpath */
		SAXTransformerFactory stf = (SAXTransformerFactory)SAXTransformerFactory.newInstance();
		Templates stylesheet = stf.newTemplates(xsltSrce);
 		Transformer transformer = stylesheet.newTransformer();
 		transformer.transform(xmlSrce, result);
 		
        BufferedReader in = new BufferedReader(new FileReader(tmpFile));
        String resStr = "";
        String str = in.readLine();
        while (str != null && str.length() > 0) {
        	resStr += str;
        	str = in.readLine();
        }
        in.close();
		assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?><endpoints xmlns=\"http://java.sun.com/xml/ns/jax-ws/ri/runtime\" version=\"2.0\">   <endpoint name=\"lsfileaeWebService\"             implementation=\"com.legstar.test.cixs.lsfileae.LsfileaeImpl\"             url-pattern=\"/lsfileae\"/></endpoints>", resStr);
	}
	
	class MyHandler extends org.xml.sax.helpers.DefaultHandler {
		
		public void startElement(String uri, String localName, String qName,
				org.xml.sax.Attributes attributes) throws SAXException
		{
			parsedData += qName.toString();
		}
	}
}
