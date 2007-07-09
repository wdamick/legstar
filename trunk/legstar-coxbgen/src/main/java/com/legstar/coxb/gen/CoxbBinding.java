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
package com.legstar.coxb.gen;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.tools.ant.BuildException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.legstar.xslt.XSLTException;
import com.legstar.xslt.XSLTParameter;
import com.legstar.xslt.XSLTransform;

/**
 * This class contains the logic to generate binding classes using
 * previously generated XML and an XSL transform.
 * 
 * @author Fady Moussallam
 * 
 */
public class CoxbBinding {

	/** The XSL transform for binding classes. */
	private static final String  BINDING_STYLE = "/xslt/coxb-bind.xsl";
	
	/** Tag enclosing a complex element description. */
	private static final String COXB_TYPE = "coxb-type";
	
	/** Tag enclosing a complex element jaxb type package. */
	private static final String JAXB_PACKAGE_ATTR = "jaxb-package";
	
	/** Attribute giving the custom choice selector marshaling class name. */
	private static final String MARSHAL_SCN_ATTR =
		"marshalChoiceStrategyClassName";
	
	/** Attribute giving the custom choice selector unmarshaling class name. */
	private static final String UNMARSHAL_SCN_ATTR =
		"unmarshalChoiceStrategyClassName";
	
	/** The XSLT parameter for existence of marchal choice selector. */
	private static final String  MARSHAL_CHOICE_EXIST =
		"marshal-choice-strategy-classname-exists";
	
	/** The XSLT parameter for existence of unmarchal choice selector. */
	private static final String  UNMARSHAL_CHOICE_EXIST =
		"unmarshal-choice-strategy-classname-exists";
	
	/** CoxbBinding uses a single XSL stylesheet associated with this
	 *  transformer. */
	private static XSLTransform mTransformer;
	
	/**
	 * No-arg constructor.
	 * 
	 * @throws XSLTException if environment is not setup
	 */
	public CoxbBinding() throws XSLTException {
		if (mTransformer == null) {
			/* XSLT style sheet is a resource within the jar */
			InputStream xsltStream  = getClass().getResourceAsStream(
					BINDING_STYLE);
			mTransformer = new XSLTransform(xsltStream);
		}
	}
	
	/**
	 * Create binding classes for a JAXB object. Some of the generated classes
	 * might be customizable, in which case we first chack that a version
	 * already exists to avoid losing any custom code.
	 * 
	 * @param bindingXMLFile the descriptor previously generated
	 * @param targetDirName the target location name for the generated classes
	 * @throws XSLTException if binding classes cannot be generated
	 */
	public final void createBinding(
			final String bindingXMLFile,
			final String targetDirName) throws XSLTException {
		
		/* Check for existence of customized code */
		CustomizableClassExistence cce =
			customizableArtifactsExits(targetDirName, bindingXMLFile);
		
		/* Pass that information to the XSL via parameters */
		Vector < XSLTParameter > params = new Vector < XSLTParameter >();

		XSLTParameter targetMCEParm = new XSLTParameter();
		targetMCEParm.setName(MARSHAL_CHOICE_EXIST);
		targetMCEParm.setExpression((new Boolean(
				cce.marshalChoiceStrategyFileExists)).toString());
		params.add(targetMCEParm);
		
		XSLTParameter targetUCEParm = new XSLTParameter();
		targetUCEParm.setName(UNMARSHAL_CHOICE_EXIST);
		targetUCEParm.setExpression((new Boolean(
				cce.unmarshalChoiceStrategyFileExists)).toString());
		params.add(targetUCEParm);
		
		/* XSLT style sheet is a resource within the jar */
		InputStream xsltStream  = getClass().getResourceAsStream(BINDING_STYLE);
		if (xsltStream == null) {
			throw (new XSLTException("Unable to locate resource "
					+ BINDING_STYLE));
		}
		mTransformer.setParams(params);
		mTransformer.transform(bindingXMLFile,
				new File(targetDirName + '/' + "dummy").toURI().toString());
	}
	
	/**
	 * Check existence of each of the customizable class names to prevent
	 * overwriting custom code.
	 * @param targetDir target folder location
	 * @param bindingXMLFile descriptor file
	 * @return a class holding true/false for each customizable class
	 * @throws XSLTException if existence cannot be verified
	 */
	private CustomizableClassExistence customizableArtifactsExits(
			final String targetDir,
			final String bindingXMLFile) throws XSLTException {

		CustomizableClassExistence cce = new CustomizableClassExistence();
		String jaxbPackageName = null;
		String marshalChoiceStrategyClassName = null;
		String unmarshalChoiceStrategyClassName = null;

    	DocumentBuilderFactory docBuilderFactory =
    		DocumentBuilderFactory.newInstance();
    	DocumentBuilder docBuilder;
		try {
			docBuilderFactory.setNamespaceAware(false);
			docBuilder = docBuilderFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(new File(bindingXMLFile));
			NodeList listOfElements = doc.getElementsByTagName(COXB_TYPE);
			if (listOfElements == null || listOfElements.getLength() == 0) {
				throw (new BuildException(
						"Empty descriptor file"));
			}
			Element coxbType = (Element) listOfElements.item(0);
			marshalChoiceStrategyClassName =
				coxbType.getAttribute(MARSHAL_SCN_ATTR);
			unmarshalChoiceStrategyClassName =
				coxbType.getAttribute(UNMARSHAL_SCN_ATTR);

			jaxbPackageName = coxbType.getAttribute(JAXB_PACKAGE_ATTR);
			if (jaxbPackageName == null) {
				throw (new XSLTException(
				"Descriptor file must define a jaxb type package"));
			}
			
			cce.marshalChoiceStrategyFileExists =
					classExists(targetDir,
							jaxbPackageName, marshalChoiceStrategyClassName);
			
			cce.unmarshalChoiceStrategyFileExists =
				classExists(targetDir,
						jaxbPackageName, unmarshalChoiceStrategyClassName);
			
		} catch (ParserConfigurationException e) {
			throw (new XSLTException(
					"ParserConfigurationException " + e.getMessage()));
		} catch (SAXException e) {
			throw (new XSLTException(
					"SAXException " + e.getMessage()));
		} catch (IOException e) {
			throw (new XSLTException(
					"IOException " + e.getMessage()));
		}
		
		return cce;
		
	}
	
	/**
	 * Check the existence of java class.
	 * @param targetDir main target folder
	 * @param defaultPackageName a default package name if class not fully
	 *  qualified
	 * @param className the java class name
	 * @return true if the file exists
	 */
	private boolean classExists(
			final String targetDir,
			final String defaultPackageName,
			final String className) {
		
		if (className == null || className.length() == 0) {
			return false;
		}
		File file = new File(
				qualifiedJavaFile(targetDir,
						defaultPackageName, className));
		return file.exists();
	}
	
	/**
	 * This class indicates which customizable files already exist on the
	 * file system.
	 */
	private class CustomizableClassExistence {
		/** Marshal choice selector. */
		boolean marshalChoiceStrategyFileExists = false;
		/** Unmarshal choice selector. */
		boolean unmarshalChoiceStrategyFileExists = false;
	}
	
	/**
	 * Determines the physical location (file name) of a java class source.
	 * @param targetDir main target folder
	 * @param defaultPackageName a default package name if class not fully
	 *  qualified
	 * @param className the java class name
	 * @return the physical java source file name
	 */
	private String qualifiedJavaFile(
			final String targetDir,
			final String defaultPackageName,
			final String className) {

		StringBuffer qualifiedName = new StringBuffer();
		String packageName = defaultPackageName;
		String fileName = className;
		
		/* First check if className is qualified. If true, use its package
		 * name rather than the default one.  */
		int dotLoc = className.lastIndexOf('.');
		if (dotLoc != -1) {
			packageName = className.substring(0, dotLoc);
			fileName = className.substring(dotLoc + 1);
		}
		
		/* Create the path name to the file */
		if (targetDir != null && targetDir.length() > 0) {
			qualifiedName.append(targetDir);
			if (targetDir.charAt(targetDir.length() - 1) != '/') {
				qualifiedName.append('/');
			}
		}
		if (packageName != null && packageName.length() > 0) {
			qualifiedName.append(packageName.replace('.', '/'));
		}
		
		if (qualifiedName.length() > 0) {
			qualifiedName.append('/');
		}
		qualifiedName.append(fileName);
		qualifiedName.append(".java");
		
		return qualifiedName.toString();
	}
}
