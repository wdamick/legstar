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
package com.legstar.xsdc.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.apache.tools.ant.BuildException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.legstar.xsdc.gen.XsdCobolAnnotator;

import junit.framework.TestCase;

public class XsdCobolAnnotatorTest extends TestCase {

	/** Cobol annotations namespace. */
	private static final String COBOL_NS = "http://www.legsem.com/xml/ns/coxb";
	/** Cobol annotations default prefix. */
	private static final String COBOL_PFX = "cb";
	/** XML Schema namespace. */
	private static final String XSD_NS = "http://www.w3.org/2001/XMLSchema";
	/** XML Schema default prefix. */
	private static final String XSD_PFX = "xs";
	
	/** A single Xpathfactory. */
	private XPathFactory mXpathFac = XPathFactory.newInstance();
	
	private static final String JAXB_PACKAGE_NAME = "com.legstar.test.coxb";

	
   /**
     * Invalid input XSD file should be reported.
     *
     * @throws Exception Any exception encountered
     */
	@SuppressWarnings("deprecation")
	public void testInvalidInputXsdFile() throws Exception {
    	/* No Xsd file at all */
    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
    	try {
        	xca.setTargetDir(new File("target"));
        	xca.setJaxbPackageName(JAXB_PACKAGE_NAME);
        	xca.execute();
    		fail("testInvalidInputXsdFile");
    	} catch (BuildException e) {
    		assertEquals("Invalid input XML schema", e.getMessage());
    	}
    	
    	/* Non existant xsd File */
    	xca.setInputXsdFile(new File("nonexistant.xsd"));
    	try {
    		xca.execute();
    		fail("testInvalidInputXsdFile");
    	} catch (BuildException e) {
    		assertEquals("Invalid input XML schema", e.getMessage());
    	}
    }

    /**
     * Default value should be provided for the target XSD file.
     *
     * @throws Exception Any exception encountered
     */
    public void testTargetXsdFileName() throws Exception {
    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
    	xca.setInputXsdUri(new File(
    			"src/test/resources/SimpleContentRestriction.xsd").toURI());
    	xca.setTargetDir(new File("target"));
    	xca.setJaxbPackageName(JAXB_PACKAGE_NAME);
    	try {
    		xca.execute();
    		assertEquals("SimpleContentRestriction.xsd", xca.getTargetXsdFileName());
    	} catch (BuildException e) {
    		fail(e.getMessage());
    	}
    	/* Maven builds might fail if Tomcat was not given enough time to reload
    	 * the war */
    	Thread.sleep(10000);
    	xca.setInputXsdUri(new URI("http://megamouss:8080/jaxws-cultureinfo/getinfo?xsd=1"));
    	xca.setTargetXsdFileName(null);
    	try {
    		xca.execute();
    		assertEquals("getinfo.xsd", xca.getTargetXsdFileName());
    	} catch (BuildException e) {
    		fail(e.getMessage());
    	}
    }
    
    /**
     * Cobol namespace should be added at the schema level.
     *
     * @throws Exception Any exception encountered
     */
    public void testCobolNamespaceAdded() throws Exception {
    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
    	xca.setInputXsdUri(
    			new File("src/test/resources/SimpleContentRestriction.xsd").toURI());
    	xca.setTargetDir(new File("target"));
    	xca.setJaxbPackageName(JAXB_PACKAGE_NAME);
    	try {
    		xca.execute();
            DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
            docFac.setNamespaceAware(true);
            DocumentBuilder builder = docFac.newDocumentBuilder();
            Document doc = builder.parse(new File("target/SimpleContentRestriction.xsd"));

//            /* DOM Version */
//            NodeList nodes = doc.getElementsByTagNameNS("http://www.w3.org/2001/XMLSchema", "schema");
//            assertEquals(1, nodes.getLength());
//            NamedNodeMap attrMap = nodes.item(0).getAttributes();
//            Node attr = attrMap.getNamedItem("xmlns:cb");
//            assertEquals("cb", attr.getLocalName()); 
//            assertEquals("xmlns:cb", attr.getNodeName());
//            assertEquals("http://www.legsem.com/xml/ns/coxb", attr.getTextContent());

            /* XPath version */
            XPathFactory xpathFac = XPathFactory.newInstance();
            XPath xpath = xpathFac.newXPath();
            NamespaceContextImpl nci = new NamespaceContextImpl();
            nci.addNamespace("xs", "http://www.w3.org/2001/XMLSchema");
            xpath.setNamespaceContext(nci);
            XPathExpression expr = xpath.compile("xs:schema");
            Object result = expr.evaluate(doc, XPathConstants.NODESET);
            NodeList nodes = (NodeList) result;
            assertEquals(1, nodes.getLength());
            NamedNodeMap attrMap = nodes.item(0).getAttributes();
            Node attr = attrMap.getNamedItem("xmlns:cb");
            assertEquals("cb", attr.getLocalName()); 
            assertEquals("xmlns:cb", attr.getNodeName());
            assertEquals("http://www.legsem.com/xml/ns/coxb", attr.getTextContent());
    	} catch (BuildException e) {
    		fail(e.getMessage());
    	}
    }
    
    /**
     * Check that root elements can be added to a schema.
     * @throws Exception Any exception encountered
     */
    public void testAddRootElements() throws Exception {
    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
    	xca.setInputXsdUri(new File(
    			"src/test/resources/noRootElementschema.xsd").toURI());
    	xca.setTargetDir(new File("target"));
    	xca.setJaxbPackageName(JAXB_PACKAGE_NAME);
    	Map <QName, QName> rootElements = new HashMap <QName, QName>();
    	rootElements.put(new QName("http://legsem.test","jvmQueryReply"),
    			new QName("http://legsem.test","jvmQueryReplyElement"));
    	xca.setRootElements(rootElements);
    	try {
    		xca.execute();
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader("target/noRootElementschema.xsd"));
		        StringBuffer res = new StringBuffer();
		        String str = in.readLine();
		        while (str != null) {
		        	res.append(str);
		        	str = in.readLine();
		        }
		        in.close();
				assertTrue(res.toString().contains("<xs:element name=\"jvmQueryReplyElement\" type=\"tns:jvmQueryReply\">"));
				assertTrue(res.toString().contains("<cb:cobolElement cobolName=\"jvmQueryReplyElement\" levelNumber=\"1\" type=\"GROUP_ITEM\"/>"));
				assertTrue(res.toString().contains("<cb:cobolElement byteLength=\"32\" cobolName=\"country\" levelNumber=\"3\" picture=\"X(32)\" type=\"ALPHANUMERIC_ITEM\" usage=\"DISPLAY\"/>"));
		    } catch (IOException e) {
	    		fail(e.getMessage());
		    }
    	} catch (BuildException e) {
    		fail(e.getMessage());
    	}
    }

    /**
     * Check that we can add extra annotations when we need to keep track
     * of java class names we want to bind to.
     * @throws Exception Any exception encountered
     */
    public void testMappingToJavaClassNames() throws Exception {
    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
    	xca.setInputXsdUri(new File(
    			"src/test/resources/complexAndsimpleTypesSchema.xsd").toURI());
    	xca.setTargetDir(new File("target"));
    	xca.setJaxbPackageName(JAXB_PACKAGE_NAME);
     	Map <String, String> complexTypeToJavaClassMap =
    		new HashMap <String, String>();
    	complexTypeToJavaClassMap.put("jvmQueryReply",
    			"com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply");
    	complexTypeToJavaClassMap.put("jvmQueryRequest",
		"com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest");
    	xca.setComplexTypeToJavaClassMap(complexTypeToJavaClassMap);
    	try {
    		xca.execute();
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader("target/complexAndsimpleTypesSchema.xsd"));
		        StringBuffer res = new StringBuffer();
		        String str = in.readLine();
		        while (str != null) {
		        	res.append(str);
		        	str = in.readLine();
		        }
		        in.close();
				assertTrue(res.toString().contains("<xs:element minOccurs=\"0\" name=\"reply\" type=\"tns:jvmQueryReply\">"));
				assertTrue(res.toString().contains("<xs:complexType name=\"jvmQueryReply\">"));
				assertTrue(res.toString().contains("<cb:cobolComplexType javaClassName=\"com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply\"/>"));
				assertTrue(res.toString().contains("<xs:complexType name=\"jvmQueryRequest\">"));
				assertTrue(res.toString().contains("<cb:cobolComplexType javaClassName=\"com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest\"/>"));
		    } catch (IOException e) {
	    		fail(e.getMessage());
		    }
    	} catch (BuildException e) {
    		fail(e.getMessage());
    	}
    }

    /**
     * Test that simplest attributes get added.
     *
     * @throws Exception Any exception encountered
     */
    public void testSimpleAttributesInsertion() throws Exception {
    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
    	xca.setInputXsdUri(new File(
    			"src/test/resources/singleSimpleElement.xsd").toURI());
    	xca.setTargetDir(new File("target"));
    	xca.setJaxbPackageName(JAXB_PACKAGE_NAME);
    	try {
    		xca.execute();
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader("target/singleSimpleElement.xsd"));
		        StringBuffer res = new StringBuffer();
		        String str = in.readLine();
		        while (str != null) {
		        	res.append(str);
		        	str = in.readLine();
		        }
		        in.close();
				assertTrue(res.toString().contains("<cb:cobolElement"));
				assertTrue(res.toString().contains("cobolName=\"CreditCardNumber\""));
				assertTrue(res.toString().contains("levelNumber=\"1\""));
		    } catch (IOException e) {
	    		fail(e.getMessage());
		    }
    	} catch (BuildException e) {
    		fail(e.getMessage());
    	}
    }
 
    /**
     * Test that we can replace the target namespace.
     *
     * @throws Exception Any exception encountered
     */
    public void testNamespaceReplacement() throws Exception {
    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
    	xca.setInputXsdUri(new File(
    			"src/test/resources/complexAndSimpleTypesSchema.xsd").toURI());
    	xca.setTargetDir(new File("target"));
    	xca.setJaxbPackageName(JAXB_PACKAGE_NAME);
    	xca.setNamespace("http://a/new/namespace");
    	try {
    		xca.execute();
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader("target/complexAndSimpleTypesSchema.xsd"));
		        StringBuffer res = new StringBuffer();
		        String str = in.readLine();
		        while (str != null) {
		        	res.append(str);
		        	str = in.readLine();
		        }
		        in.close();
				assertTrue(res.toString().contains("targetNamespace=\"http://a/new/namespace\""));
				assertTrue(res.toString().contains("xmlns:tns=\"http://a/new/namespace\""));
				assertTrue(res.toString().contains("xmlns:cb=\"http://www.legsem.com/xml/ns/coxb\""));
				assertTrue(res.toString().contains("xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\""));
				assertTrue(res.toString().contains("<xs:element minOccurs=\"0\" name=\"reply\" type=\"tns:jvmQueryReply\">"));
				assertTrue(res.toString().contains("<xs:element name=\"jvmQueryAggregateElement\" type=\"tns:jvmQueryAggregate\">"));
		    } catch (IOException e) {
	    		fail(e.getMessage());
		    }
    	} catch (BuildException e) {
    		fail(e.getMessage());
    	}
    }
 
    /**
     * Simple types (Primitive).
     *
     * @throws Exception Any exception encountered
     */
    public void testAllSimpleTypes() throws Exception {
    	Document doc = getDocument("allSimpleTypes.xsd", null);
        NamedNodeMap attrMap;
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'PrimitiveString']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("X(32)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("DISPLAY", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("32", attrMap.getNamedItem("byteLength").getTextContent());
       
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'PrimitiveBoolean']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(1)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("2", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("false", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("1", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedPositiveInteger']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(9)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("4", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("false", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedNegativeInteger']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(9)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("4", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedNonNegativeInteger']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(9)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("4", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("false", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedNonpositiveInteger']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(9)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("4", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedUnsignedShort']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(4)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("2", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("false", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("4", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedUnsignedLong']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(18)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("8", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("false", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("18", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedUnsignedInt']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(9)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("4", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("false", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedLong']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(18)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("8", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("18", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedShort']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(4)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("2", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("4", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedInt']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(9)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("4", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
            
        /* TODO Check against XML schema spec */
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'DerivedInteger']", doc);
        assertEquals("BINARY_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(9)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-5", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("4", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'PrimitiveDecimal']", doc);
        assertEquals("PACKED_DECIMAL_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(7)V9(2)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-3", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("5", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        assertEquals("2", attrMap.getNamedItem("fractionDigits").getTextContent());
       
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'PrimitiveFloat']", doc);
        assertEquals("SINGLE_FLOAT_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("COMP-1", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("4", attrMap.getNamedItem("byteLength").getTextContent());
       
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'PrimitiveDouble']", doc);
        assertEquals("DOUBLE_FLOAT_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("COMP-2", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("8", attrMap.getNamedItem("byteLength").getTextContent());
       
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'PrimitiveHexBin']", doc);
        assertEquals("OCTET_STREAM_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("X(32)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("DISPLAY", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("32", attrMap.getNamedItem("byteLength").getTextContent());
       
    }
    
    /**
     * Inlined simple types.
     *
     * @throws Exception Any exception encountered
     */
    public void testSimpleTypesInlined() throws Exception {
    	Document doc = getDocument("credit-card-faults.xsd", null);
        NamedNodeMap attrMap;
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'CreditCardNumber']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());
        
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'CreditCardType']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());
    }
    
    /**
     * String types.
     *
     * @throws Exception Any exception encountered
     */
    public void testStringTypes() throws Exception {
    	Document doc = getDocument("StringTypes.xsd", null);
        NamedNodeMap attrMap;
    	
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'myZipCode']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("X(5)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("5", attrMap.getNamedItem("byteLength").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'myWhiteSpace']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'layoutComponent']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());

    }

    /**
     * Decimal types.
     *
     * @throws Exception Any exception encountered
     */
    public void testDecimalTypes() throws Exception {
    	Document doc = getDocument("DecimalTypes.xsd", null);
        NamedNodeMap attrMap;
    	
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'CreditCardNumber']", doc);
        assertEquals("PACKED_DECIMAL_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(7)V9(2)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-3", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("5", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        assertEquals("2", attrMap.getNamedItem("fractionDigits").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'accBalance']", doc);
        assertEquals("PACKED_DECIMAL_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(6)V9(3)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-3", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("5", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("9", attrMap.getNamedItem("totalDigits").getTextContent());
        assertEquals("3", attrMap.getNamedItem("fractionDigits").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'myHeight']", doc);
        assertEquals("PACKED_DECIMAL_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("9(2)V9(1)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("COMP-3", attrMap.getNamedItem("usage").getTextContent());
        assertEquals("2", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("true", attrMap.getNamedItem("signed").getTextContent());
        assertEquals("3", attrMap.getNamedItem("totalDigits").getTextContent());
        assertEquals("1", attrMap.getNamedItem("fractionDigits").getTextContent());

    }

    /**
     * Deep hierarchy of types (test recursive search for primitive type).
     *
     * @throws Exception Any exception encountered
     */
    public void testDeepTypeHierarchy() throws Exception {
    	Document doc = getDocument("DeepHierarchySimpleTypes.xsd", null);
        NamedNodeMap attrMap;
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'myZipCode']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("X(5)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("5", attrMap.getNamedItem("byteLength").getTextContent());
    }
    
    /**
     * Complex type types.
     *
     * @throws Exception Any exception encountered
     */
    public void testComplexWithinComplexTypes() throws Exception {
    	Document doc = getDocument("complexWithinComplex.xsd", null);
        NamedNodeMap attrMap;
    	
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'accInfo']", doc);
        assertEquals("accInfo", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("1", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("GROUP_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'custInfo']", doc);
        assertEquals("custInfo", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("3", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("GROUP_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'accName']", doc);
        assertEquals("accName", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("3", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'accName']", doc);
        assertEquals("accName", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("3", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'accNumber']", doc);
        assertEquals("accNumber", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("3", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'accBalance']", doc);
        assertEquals("accBalance", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("3", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("PACKED_DECIMAL_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'customerSurname']", doc);
        assertEquals("customerSurname", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("5", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'customerForname']", doc);
        assertEquals("customerForname", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("5", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());

        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'branchCode']", doc);
        assertEquals("branchCode", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("5", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());

    }
    
    /**
     * Array types.
     *
     * @throws Exception Any exception encountered
     */
    public void testArrays() throws Exception {
    	Document doc = getDocument("Arrays.xsd", null);
        NamedNodeMap attrMap;
    	
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'accInfo']", doc);
        assertEquals("accInfo", attrMap.getNamedItem("cobolName").getTextContent());
        assertEquals("1", attrMap.getNamedItem("levelNumber").getTextContent());
        assertEquals("GROUP_ITEM", attrMap.getNamedItem("type").getTextContent());
    }
    /**
     * Deep hierarchy of types (test recursive search for primitive type).
     *
     * @throws Exception Any exception encountered
     */
    public void testMSNSearch() throws Exception {
    	Document doc = getDocument("MSNSearch.xsd", "Type");
        NamedNodeMap attrMap;
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'Flags']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("X(32)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("32", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("10", attrMap.getNamedItem("maxOccurs").getTextContent());
    }
    
    /**
     * same as MSNSearch except that we grab the schema directly from the wsdl.
     *
     * @throws Exception Any exception encountered
     */
    public void testMSNSearchFromWsdl() throws Exception {
    	Document doc = getDocument("MSNSearch.wsdl", "Type");
        NamedNodeMap attrMap;
        attrMap = getAttributesFromCobolAnnotation("//xs:element[@name = 'Flags']", doc);
        assertEquals("ALPHANUMERIC_ITEM", attrMap.getNamedItem("type").getTextContent());
        assertEquals("X(32)", attrMap.getNamedItem("picture").getTextContent());
        assertEquals("32", attrMap.getNamedItem("byteLength").getTextContent());
        assertEquals("10", attrMap.getNamedItem("maxOccurs").getTextContent());
    }

    /** Helper method creates a DOM document from an XSD file name */
    private Document getDocument(String xsdFileName, String jaxbTypeClassesSuffix) {
    	Document doc = null;
    	try {
	    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
	    	xca.setInputXsdUri(new File(
	    			"src/test/resources/" + xsdFileName).toURI());
	    	xca.setTargetDir(new File("target"));
	    	xca.setJaxbPackageName(JAXB_PACKAGE_NAME);
			xca.setJaxbTypeClassesSuffix(jaxbTypeClassesSuffix);
			xca.execute();
	        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
	        docFac.setNamespaceAware(true);
	        DocumentBuilder builder = docFac.newDocumentBuilder();
	        doc = builder.parse(new File("target/" + xca.getTargetXsdFileName()));
    	} catch (ParserConfigurationException e) {
    		fail(e.getMessage());
    	} catch (SAXException e) {
    		fail(e.getMessage());
		} catch (IOException e) {
    		fail(e.getMessage());
		}
        return doc;
    }

    /** Helper method to get Cobol annotation for a particular cobol item.
     * In order to locate the annotation, we need an xpath to the parent
     * element. */
    private NamedNodeMap getAttributesFromCobolAnnotation(
    		String xPathParent, Document doc) {
    	NamedNodeMap attrMap = null;
    	try {
	        XPath xpath = mXpathFac.newXPath();
            NamespaceContextImpl nci = new NamespaceContextImpl();
            nci.addNamespace(COBOL_PFX, COBOL_NS);
            nci.addNamespace(XSD_PFX, XSD_NS);
	        xpath.setNamespaceContext(nci);
	        XPathExpression expr = xpath.compile(xPathParent + "/"
	        		+ XSD_PFX + ":annotation" + "/"
	        		+ XSD_PFX + ":appinfo" + "/"
	        		+ COBOL_PFX + ":cobolElement");
	        Object result = expr.evaluate(doc, XPathConstants.NODESET);
	        NodeList nodes = (NodeList) result;
	        assertEquals(1, nodes.getLength());
	        attrMap = nodes.item(0).getAttributes();
    	} catch (XPathExpressionException e) {
    		fail(e.getMessage());
		}
    	return attrMap;
    }
    
}
