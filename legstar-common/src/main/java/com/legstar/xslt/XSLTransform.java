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
package com.legstar.xslt;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Iterator;
import java.util.Vector;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.tools.ant.BuildException;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

import net.sf.saxon.TransformerFactoryImpl;

/**
 * This class implements a single method designed to apply an XSLT transform
 * on an XML file that contains XInclude. The standard XSL engines do not
 * resolve those XIncludes. Furthermore, since we need XSLT 2.0 transforms,
 * we need saxon until the default transform engines that come with the JDK
 * support XSLT 2.0.
 * Normally, we shouldn't explicitly reference the saxon implementation but
 * rather let JAXP sort things out like so:
 *			SAXTransformerFactory stf =
 *				(SAXTransformerFactory) SAXTransformerFactory.newInstance();
 * The problem of doing this is that the default transform engine gets loaded
 * very early in the process of running this ANT task. As a consequence, all
 * dependant classes which might be invoked from the XSLT engine (we use
 * saxon extensions) also need to be added to the classpath very early which
 * is ugly. By referencing saxon explicitly in this class, saxon can appear
 * in the classpath much later, for instance in the taskdef for the ant task.
 * 
 * @author Fady Moussallam
 * 
 */
public class XSLTransform {


	/**
     * Transform method.
     * 
     * @param xmlFile the XML file on which the transform should apply
     * @param xsltStream the style sheet describing the transform as a URL
     * @param resultFile the result of the transformation process
     * @param params the parameters passed to the style sheet
     */
    public final void transform(
    		final String xmlFile,
    		final InputStream xsltStream,
    		final String resultFile,
    		final Vector < XSLTParameter > params) {

    	StreamResult  result = new StreamResult(resultFile);
		StreamSource  xsltSrce = new StreamSource(xsltStream);
  		
		try {
			SAXParserFactory spf =
				(SAXParserFactory) SAXParserFactory.newInstance();
			spf.setXIncludeAware(true);
			spf.setNamespaceAware(true);
			XMLReader reader;
			reader = spf.newSAXParser().getXMLReader();
			SAXSource xmlSrce =
				new SAXSource(reader, new InputSource(xmlFile));

			/* Make sure we have an XSLT 2.0 transformerFactory in the
			 *  classpath */
			TransformerFactoryImpl stf =
				new TransformerFactoryImpl();
			Templates stylesheet;
			stylesheet = stf.newTemplates(xsltSrce);
			Transformer transformer;
			transformer = stylesheet.newTransformer();
			
			/* Pass on any parameters */
			if (params != null) {
		        for (Iterator it = params.iterator(); it.hasNext();) {
		        	XSLTParameter param = (XSLTParameter) it.next();
		            transformer.setParameter(
		            		param.getName(), param.getExpression());
		        }
			}
	        
			/* Perform transformation */
			transformer.transform(xmlSrce, result);
		} catch (SAXException e) {
			e.printStackTrace();
			throw (new BuildException("SAXException " + e.getMessage()));
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			throw (new BuildException(
					"ParserConfigurationException " + e.getMessage()));
		} catch (TransformerConfigurationException e) {
			e.printStackTrace();
			throw (new BuildException(
					"TransformerConfigurationException " + e.getMessage()));
		} catch (TransformerException e) {
			e.printStackTrace();
			throw (new BuildException(
					"TransformerException " + e.getMessage()));
		}
    }
	/**
     * Transform method.
     * 
     * @param xmlFile the XML file on which the transform should apply
     * @param xsltResource the style sheet describing the transform as a URL
     * @param resultFile the result of the transformation process
     * @param params the parameters passed to the style sheet
     */
    public final void transform(
    		final String xmlFile,
    		final URL xsltResource,
    		final String resultFile,
    		final Vector < XSLTParameter > params) {

		InputStream xsltStream;
		try {
			xsltStream = xsltResource.openStream();
		} catch (IOException e) {
			e.printStackTrace();
			throw (new BuildException(
					"IOException " + e.getMessage()));
		}
		transform(xmlFile, xsltStream, resultFile, params);
    }
}
