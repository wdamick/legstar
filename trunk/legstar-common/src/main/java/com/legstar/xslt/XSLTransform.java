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

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

import net.sf.saxon.TransformerFactoryImpl;

/**
 * This class implements methods to apply an XSLT transform on an XML file 
 * that contains XInclude. The standard XSL engines (as of JDK 6) do not
 * resolve those XIncludes. Furthermore, since we need XSLT 2.0 transforms,
 * we need saxon until the default transform engines that come with the JDK
 * support XSLT 2.0.
 * Normally, we shouldn't explicitly reference the saxon implementation but
 * rather let JAXP sort things out like so:
 *			SAXTransformerFactory stf =
 *				(SAXTransformerFactory) SAXTransformerFactory.newInstance();
 * The problem of doing this is that the default transform engine gets loaded
 * very early in the process of running ANT tasks. As a consequence, all
 * dependant classes which might be invoked from the XSLT engine (we use
 * saxon extensions) also need to be added to the classpath very early which
 * is ugly. By referencing saxon explicitly in this class, saxon can appear
 * in the classpath much later, for instance in the taskdef for the ant task.
 * 
 * @author Fady Moussallam
 * 
 */
public class XSLTransform {


	/** The current XSL transformer. */
	private Transformer mTransformer;
	
	/** Parameters to pass on the next transform operation. */
	private Vector < XSLTParameter > mParams;
	
	/**
	 * An XSLTransform can be created for a stylesheet. This allows
	 * the stylesheet to be compiled only once and used multiple times.
	 * @param xsltStream an input stream to the XSL stylesheet
	 * @throws XSLTException if stylesheet cannot be loaded
	 */
	public XSLTransform(final InputStream xsltStream) throws XSLTException {
		try {
			StreamSource  xsltSrce = new StreamSource(xsltStream);
			TransformerFactoryImpl stf = new TransformerFactoryImpl();
			Templates stylesheet = stf.newTemplates(xsltSrce);
			mTransformer = stylesheet.newTransformer();
		} catch (TransformerConfigurationException e) {
			throw new XSLTException(e.getMessage());
		}
	}
	
	/**
	 * An XSLTransform can be created for a stylesheet. This allows
	 * the stylesheet to be compiled only once and used multiple times.
	 * @param xsltResourceName the XSL stylesheet as a resource in classpath
	 * @throws XSLTException if stylesheet cannot be loaded
	 */
	public XSLTransform(final String xsltResourceName) throws XSLTException {
		try {
			URL xsltResource  = getClass().getResource(xsltResourceName);
			if (xsltResource == null) {
				throw (new XSLTException("Unable to locate resource "
						+ xsltResourceName));
			}
			InputStream xsltStream = xsltResource.openStream();
			TransformerFactoryImpl stf = new TransformerFactoryImpl();
			Templates stylesheet = stf.newTemplates(
					new StreamSource(xsltStream));
			mTransformer = stylesheet.newTransformer();
			xsltStream.close();
		} catch (TransformerConfigurationException e) {
			throw new XSLTException(e.getMessage());
		} catch (IOException e) {
			throw new XSLTException(e.getMessage());
		}
	}
	
	/**
     * Transform method.
     * 
     * @param xmlFile the XML file on which the transform should apply
     * @param resultFile the result of the transformation process
	 * @throws XSLTException if transformation fails
     */
    public final void transform(
    		final String xmlFile,
    		final String resultFile) throws XSLTException {

    	StreamResult  result = new StreamResult(resultFile);
  		
		try {
			SAXParserFactory spf =
				(SAXParserFactory) SAXParserFactory.newInstance();
			spf.setXIncludeAware(true);
			spf.setNamespaceAware(true);
			XMLReader reader;
			reader = spf.newSAXParser().getXMLReader();
			SAXSource xmlSrce =
				new SAXSource(reader, new InputSource(xmlFile));

			/* Pass on any parameters */
			if (mParams != null) {
		        for (Iterator it = mParams.iterator(); it.hasNext();) {
		        	XSLTParameter param = (XSLTParameter) it.next();
		        	mTransformer.setParameter(
		            		param.getName(), param.getExpression());
		        }
			}
	        
			/* Perform transformation */
			mTransformer.transform(xmlSrce, result);
		} catch (SAXException e) {
			e.printStackTrace();
			throw (new XSLTException(e));
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			throw (new XSLTException(e));
		} catch (TransformerConfigurationException e) {
			throw (new XSLTException(e));
		} catch (TransformerException e) {
			throw (new XSLTException(e));
		}
    }

	/**
	 * @return the XSL transformer
	 */
	public final Transformer getTransformer() {
		return mTransformer;
	}

	/**
	 * @param transformer the XSL transformer to set
	 */
	public final void setTransformer(final Transformer transformer) {
		mTransformer = transformer;
	}

	/**
	 * @return the parameters to pass on the next transform operation
	 */
	public final Vector < XSLTParameter > getParams() {
		return mParams;
	}

	/**
	 * @param params the next transform operation parameters to set
	 */
	public final void setParams(final Vector < XSLTParameter > params) {
		mParams = params;
	}
}
