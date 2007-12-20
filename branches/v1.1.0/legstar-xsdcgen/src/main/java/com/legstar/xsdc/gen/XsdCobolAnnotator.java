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
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaFractionDigitsFacet;
import org.apache.ws.commons.schema.XmlSchemaLengthFacet;
import org.apache.ws.commons.schema.XmlSchemaObject;
import org.apache.ws.commons.schema.XmlSchemaObjectCollection;
import org.apache.ws.commons.schema.XmlSchemaPatternFacet;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.apache.ws.commons.schema.XmlSchemaSimpleType;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeList;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeRestriction;
import org.apache.ws.commons.schema.XmlSchemaTotalDigitsFacet;
import org.apache.ws.commons.schema.constants.Constants;
import org.apache.ws.commons.schema.utils.NamespaceMap;
import org.apache.ws.commons.schema.utils.NamespacePrefixList;
import org.w3c.dom.Attr;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.legstar.coxb.CobolType;
import com.legstar.coxb.CobolMarkup;

/**
 * This Ant Task maps XML schema elements with Cobol data types. The result is
 * a new XML Schema with special annotations that can be used to map XML to
 * legacy data.
 *
 */
public class XsdCobolAnnotator extends Task {
	
	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(XsdCobolAnnotator.class);
	
	/* ====================================================================== */
	/* = Properties section                                                 = */
	/* ====================================================================== */
	/** The original XSD file to annotate.*/
	private File mInputXsdFile;

	/** The target directory where annotated XSD will be created. */
	private File mTargetDir;
	
	/** The target annotated XSD file name. */
	private String mTargetXsdFileName;
	
	/** Suffix to be added to JAXB classes names for XML schema types. This 
	 * is used to disambiguate java object names when elements and types
	 * have the same names (frequently used by Microsoft).*/
	private String mJaxbTypeClassesSuffix;
	
	/** For JAXB, the generated XML Schema contains a special annotation that
	 * determines the JAXB classes package name. */
	private String mJaxbPackageName;
	
	/** This builder is used for annotation markup elements. */
	private final DocumentBuilder mDb;

	/** Associates a cobol type to an XSD primitive type. */
	private XsdCobolTypeMap mTypeMap = new XsdCobolTypeMap();
	
	/** Used to build valid cobol names from java names. */
	private CobolNameResolver mNameResolver;
	
	/** Parameters that can be externally modified. */
	private Properties mOptions;
	
	/* ====================================================================== */
	/* = Constants section                                                  = */
	/* ====================================================================== */
	/** XML Schema namespace. */
	private static final String XSD_NS = "http://www.w3.org/2001/XMLSchema";
	
	/** SOAP namespace. */
	private static final String SOAP_NS =
		"http://schemas.xmlsoap.org/wsdl/soap/";
	
	/** WSDL namespace. */
	private static final String WSDL_NS = "http://schemas.xmlsoap.org/wsdl/";

	/** WS-Addressing namespace. */
	private static final String ADDRESSING_NS =
		"http://schemas.xmlsoap.org/ws/2004/08/addressing";
	
	/** Namespaces namespace. */
	private static final String NS_NS = "http://www.w3.org/2000/xmlns/";
	
	/** Cobol annotations namespace. */
	private static final String COBOL_NS = "http://www.legsem.com/xml/ns/coxb";
	
	/** Cobol annotations default prefix. */
	private static final String COBOL_PFX = "cb";
	
	/** Jaxb annotations namespace. */
	private static final String JAXB_NS = "http://java.sun.com/xml/ns/jaxb";
	
	/** Jaxb annotations default prefix. */
	private static final String JAXB_PFX = "jaxb";
	
	/** Cobol annotation parent element name. */
	private static final String COBOL_PARENT_ELN = "cb:cobolElements";
	
	/** Cobol annotation element name. */
	private static final String COBOL_ELN = "cb:cobolElement";
	
	/** JAXB annotation parent element name. */
	private static final String JAXB_PARENT_ELN = "jaxb:jaxbElements";
	
	/** JAXB annotation for schema binding parameters. */
	private static final String JAXB_SCHEMAB_ELN = "jaxb:schemaBindings";
	
	/** JAXB annotation for target package name. */
	private static final String JAXB_PKG_ELN = "jaxb:package";
	
	/** JAXB annotation for name transformation. */
	private static final String JAXB_XFORM_ELN = "jaxb:nameXmlTransform";
	
	/** JAXB annotation for a type name transformation. */
	private static final String JAXB_TYPENAME_ELN = "jaxb:typeName";
	
	/** Default target JAXB classes package name prefix. */
	public static final String DEFAULT_JAXB_PKG_PREFIX
		= "com.legstar.test.coxb";
	
	/** Maximum size of a Cobol item name. */
	public static final int MAX_COBOLNAME_LEN = 30;
	
	/** The cobol reserved words substitution file name. */
	private static final String OPTIONS_FILE_NAME = "xsdcoptions.properties";
	
	/**
	 * At construction time, a DOM factory is created. The DOM factory is later
	 * used to create DOM documents which serve as home for all the annotation
	 * markup nodes that will be created during the annotation process.
	 * We also load cobol name substitution lists and options.
	 */
	public XsdCobolAnnotator() {
		try {
			DocumentBuilderFactory docFac =
				DocumentBuilderFactory.newInstance();
			docFac.setNamespaceAware(true);
			mDb = docFac.newDocumentBuilder();
			mNameResolver = new CobolNameResolver();
			mOptions = XsdcUtil.loadFromPropFile(this.getClass(),
					OPTIONS_FILE_NAME);
		} catch (ParserConfigurationException e) {
			throw (new BuildException(e));
		} catch (CobolNameResolverException e) {
			throw (new BuildException(e));
		} catch (IOException e) {
			throw (new BuildException(e));
		}
	}
	
	/**
	 *  The ant execute method. Generates a new annotated schema.
	 */
    public final void execute() {
		if (LOG.isDebugEnabled()) {
			LOG.debug("XML Schema Cobol annotation started");
		}
    	checkInput();
    	XmlSchema schema = getSchema();
    	annotateSchema(schema);
    	OutputStream out = getOutputStream();
        XmlSchemaObjectCollection items = schema.getItems();
 
        /* Process each element in the input Schema */
        try {
	        for (int i = 0; i < items.getCount(); i++) {
	            XmlSchemaObject obj = items.getItem(i);
	            if (obj instanceof XmlSchemaElement) {
	            	annotateElement(schema, (XmlSchemaElement) obj, 1);
	            }
	        }
		} catch (XsdCobolAnnotatorException e) {
			throw (new BuildException(e));
		}
        schema.write(out);
		if (LOG.isDebugEnabled()) {
			LOG.debug("XML Schema Cobol annotation ended");
		}
    }

    /**
     * Checks that properties set are valid.
     */
    private void checkInput() {
    	
		if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput started");
			LOG.debug("   Source Xsd file      = "
					+ ((mInputXsdFile == null) ? null
							: mInputXsdFile.getAbsolutePath()));
			LOG.debug("   Target directory     = " + mTargetDir);
			LOG.debug("   Target Xsd file name = " + mTargetXsdFileName);
		}
    	/* Check that we have a valid input XML schema.  */
    	if (mInputXsdFile == null || !mInputXsdFile.exists()) {
			throw (new BuildException(
					"Invalid input XML schema file"));
    	}
    	
    	/* Check that we have a valid target directory.  */
    	if (mTargetDir == null) {
			throw (new BuildException(
					"You must provide a target directory"));
    	}
    	if (!mTargetDir.exists()) {
			throw (new BuildException(
					"Directory " + mTargetDir + " does not exist"));
    	}
    	if (!mTargetDir.isDirectory() || !mTargetDir.canWrite()) {
			throw (new BuildException(
					mTargetDir + " is not a directory or is not writable"));
    	}
    	
    	/* Set a valid target annotated XSD file name */
    	if (mTargetXsdFileName == null || mTargetXsdFileName.length() == 0) {
    		mTargetXsdFileName = mInputXsdFile.getName();
    		/* If there is no extension or extension is not xsd, add xsd as
    		 * the extension. */
            int p = mTargetXsdFileName.lastIndexOf('.');
            if (p > 0) {
            	String ext = mTargetXsdFileName.substring(
            			p, mTargetXsdFileName.length());
            	if (ext.compareToIgnoreCase(".xsd") != 0) {
            		mTargetXsdFileName = mTargetXsdFileName + ".xsd";
            	}
            }
   	}
		if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput ended");
		}
    }
    
    /**
     * Loads the input XML Schema either from an XSD file or a WSDL file.
     * @return a ws-commons XmlSchema instance. 
     */
	private XmlSchema getSchema() {
    	
		if (LOG.isDebugEnabled()) {
			LOG.debug("getSchema started");
		}
    	/* Load the input schema */
    	XmlSchemaCollection schemaCol = new XmlSchemaCollection();
    	XmlSchema schema;
		try {
	        Document doc = mDb.parse(mInputXsdFile);
	        
	        /* Get the root element (skipping any comments) */
	        Node root = doc.getFirstChild();
	        while (root != null && root instanceof Comment) {
	        	root = root.getNextSibling();
	        }
	        if (root == null) {
	        	throw new BuildException("File " + mInputXsdFile
	        			+ " does not contain an XML schema");
	        }

	        /* Look for an XML schema node */
	        NodeList nodes  = doc.getElementsByTagNameNS(XSD_NS, "schema");
	        if (nodes == null || nodes.getLength() == 0) {
	        	throw new BuildException("File " + mInputXsdFile
	        			+ " does not contain an XML schema");
	        }
	        if (nodes.getLength() > 1) {
	        	LOG.warn("Only the first XML schema in " + mInputXsdFile
	        			+ " will be processed");
	        }
	        Node schemaNode = nodes.item(0);
	        
	        /* In case this is a wsdl file, we store the namespace
	         * attributes of the root at the schema node level so that
	         * the schema node is complete from an XmlSchema standpoint. */
	        if (root.getLocalName().compareTo("definitions") == 0
	        		&& root.getNamespaceURI().compareTo(WSDL_NS) == 0) {
	        	
	        	for (int i = 0; i < root.getAttributes().getLength(); i++) {
	        		Attr attribute = (Attr) root.getAttributes().item(i);
	        		String namespaceUri = attribute.getNamespaceURI();
	        		String value = attribute.getValue();
	        		if (namespaceUri != null
	        				&& namespaceUri.compareTo(NS_NS) == 0) {
	        			
	        			/* Omit soap and wsdl namespaces which are not useful
	        			 * at an XML schema level. */
		        		if (value.compareTo(SOAP_NS) != 0
		        				&& value.compareTo(WSDL_NS) != 0
		        				&& value.compareTo(ADDRESSING_NS) != 0) {
		        			((Element) schemaNode).setAttributeNodeNS(
		        					(Attr) attribute.cloneNode(true));
		        		}
	        		}
	        	}
	        }
	    	schema = schemaCol.read((Element) schemaNode);
		} catch (FileNotFoundException e) {
			throw (new BuildException(e));
		} catch (SAXException e) {
			throw (new BuildException(e));
		} catch (IOException e) {
			throw (new BuildException(e));
		}
    	
		if (LOG.isDebugEnabled()) {
			LOG.debug("getSchema ended. Target namespace = "
					+ schema.getTargetNamespace());
		}
    	return schema;
    }
    
    /**
     * Adds JAXB and COXB annotations at the schema level.
     * @param schema the current schema being generated
     */
    @SuppressWarnings("unchecked")
	private void annotateSchema(final XmlSchema schema) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("AnnotateSchema started");
		}
    	/* Add the JAXB and COXB namespaces to the target schema */
    	NamespaceMap prefixmap = new NamespaceMap();
    	NamespacePrefixList npl = schema.getNamespaceContext();
    	for (int i = 0; i < npl.getDeclaredPrefixes().length; i++) {
    		prefixmap.add(npl.getDeclaredPrefixes()[i], npl.getNamespaceURI(
    				npl.getDeclaredPrefixes()[i]));
    	}
    	/* TODO check that prefix does not conflict with an existing one.
    	 * 3 situations might occur:
    	 * 1: an existing prefix with same value exists and points to a 
    	 *    different namespace
    	 * 2: namespace is already listed with a different prefix
    	 * 3: namespace is already listed with the same prefix
    	 * */
    	prefixmap.add(COBOL_PFX, COBOL_NS);
    	prefixmap.add(JAXB_PFX, JAXB_NS);
    	schema.setNamespaceContext(prefixmap);
    	
    	/* Add JAXB extension attributes to the target schema */
    	Document doc = mDb.newDocument();
        Attr attrib = doc.createAttributeNS(JAXB_NS, "version");
        attrib.setValue("2.0");
        Map eam = (Map) schema.getMetaInfoMap().get(
        		Constants.MetaDataConstants.EXTERNAL_ATTRIBUTES);
        eam.put(new QName(JAXB_NS, "version"), attrib);
        
        attrib = doc.createAttributeNS(JAXB_NS, "extensionBindingPrefixes");
        attrib.setValue(COBOL_PFX);
        eam.put(new QName(JAXB_NS, "extensionBindingPrefixes"), attrib);
   	
    	/* Annotate the schema element with JAXB extension parameters */
    	schema.setAnnotation(getSchemaAnnotations(doc));
    	
		if (LOG.isDebugEnabled()) {
			LOG.debug("AnnotateSchema ended");
		}
        
    }
    
    /**
     * The generated schema holds JAXB annotations needed when, later on, the
     * schema is used to generated JAXB classes. The markup looks like this:
     * <pre>
     * &lt;xsd:appinfo>
     *    &lt;jaxb:globalBindings generateIsSetMethod="true"/>
	 *    &lt;jaxb:schemaBindings>
	 *       &lt;jaxb:package name="com.legstar.test.coxb.schema"/>
	 *       &lt;jaxb:nameXmlTransform>
	 *          &lt;jaxb:typeName suffix="Type" />
	 *        &lt;/jaxb:nameXmlTransform>
	 *    &lt;/jaxb:schemaBindings>
     * &lt;/xsd:appinfo>
     * </pre>
     * @param doc a DOM document to hold nodes used for annotations
     * @return a schema level annotation
     *  */
    private XmlSchemaAnnotation getSchemaAnnotations(
    		final Document doc) {
        
        Element el = doc.createElementNS(JAXB_NS, JAXB_PARENT_ELN);
        Element elsb = doc.createElementNS(JAXB_NS, JAXB_SCHEMAB_ELN);
        
        Element elpk = doc.createElementNS(JAXB_NS, JAXB_PKG_ELN);
        String name = mInputXsdFile.getName();
        int p = name.lastIndexOf('.');
        if (p > 0) {
        	name = mInputXsdFile.getName().substring(0, p);
        }
        if (mJaxbPackageName == null || mJaxbPackageName.length() == 0) {
	        elpk.setAttribute("name", DEFAULT_JAXB_PKG_PREFIX + '.' + name);
        } else {
	        elpk.setAttribute("name", mJaxbPackageName);
        }
        elsb.appendChild(elpk);
        
        if (mJaxbTypeClassesSuffix != null
        		&& mJaxbTypeClassesSuffix.length() > 0) {
	        Element eltr = doc.createElementNS(JAXB_NS, JAXB_XFORM_ELN);
	        Element eltn = doc.createElementNS(JAXB_NS, JAXB_TYPENAME_ELN);
	        eltn.setAttribute("suffix", mJaxbTypeClassesSuffix);
	        eltr.appendChild(eltn);
	        elsb.appendChild(eltr);
        }
        
        el.appendChild(elsb);
    	XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
    	XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
    	NodeList markup = el.getChildNodes();
    	appInfo.setMarkup(markup);
    	annotation.getItems().add(appInfo);
    	return annotation;
    }
    
    /**
     * Creates an output stream from the input XML schema file name.
     * @return an output stream
     */
    private OutputStream getOutputStream() {
    	OutputStream out;
    	String outPath = mTargetDir.getPath() + File.separator
    	        + mTargetXsdFileName;
    	try {
			out = new FileOutputStream(new File(outPath));
			return out;
		} catch (FileNotFoundException e) {
			throw (new BuildException(e));
		}	
    }
    
    /**
     * Main annotation process applied to an XML schema element.
     * @param schema the XML Schema being annotated
     * @param obj the XML Schema element to annotate
     * @param level the current level in the elements hierarchy. This is used
     *        to create Cobol levels with the same depth as the input XML 
     *        schema.
     * @throws XsdCobolAnnotatorException if annotation fails
     */
    public final void annotateElement(
    		final XmlSchema schema,
    		final XmlSchemaElement obj,
    		final int level) throws XsdCobolAnnotatorException {
    	/* If this element is referencing another, it might not be useful to 
    	 * annote it. */
    	if (obj.getRefName() != null) {
    		return;
    	}
		if (LOG.isDebugEnabled()) {
			LOG.debug("annotate started for element = " + obj.getName());
		}
		
    	/* Create a DOM document to hold annotation notes */
        Document doc = mDb.newDocument();
        Element el = doc.createElementNS(COBOL_NS, COBOL_PARENT_ELN);
        Element elc = doc.createElementNS(COBOL_NS, COBOL_ELN);
        setAttributes(schema, obj, elc, level);
        el.appendChild(elc);
        
    	/* Add an annotation */
    	XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
    	XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
    	NodeList markup = el.getChildNodes();
    	appInfo.setMarkup(markup);
    	annotation.getItems().add(appInfo);
    	obj.setAnnotation(annotation);
    	
		if (LOG.isDebugEnabled()) {
			LOG.debug("annotate ended for element = " + obj.getName());
		}
    }
    
    /**
     * Create a set of attributes for the Cobol annotation element depending
     * on corresponding XML schema type.
     * @param schema the XML Schema being annotated
     * @param obj the XML Schema element to annotate
     * @param elc the DOM Element representing the Cobol annotation
     * @param level the current level in the elements hierarchy. This is used
     *        to create Cobol levels with the same depth as the input XML 
     *        schema.
     * @throws XsdCobolAnnotatorException if annotation fails
     */
    private void setAttributes(
    		final XmlSchema schema,
    		final XmlSchemaElement obj,
    		final Element elc,
    		final int level) throws XsdCobolAnnotatorException {
    	
		if (LOG.isDebugEnabled()) {
			LOG.debug("setAttributes started for element  = "
					+ obj.getName());
	    	LOG.debug("   XmlSchemaElement QName          = "
	    			+ obj.getQName());
	    	LOG.debug("   XmlSchemaElement SchemaType     = "
	    			+ obj.getSchemaType());
	    	LOG.debug("   XmlSchemaElement SchemaTypeName = "
	    			+ obj.getSchemaTypeName());
	    	LOG.debug("   XmlSchemaElement MaxOccurs      = "
	    			+ obj.getMaxOccurs());
	    	LOG.debug("   XmlSchemaElement MinOccurs      = "
	    			+ obj.getMinOccurs());
	    	LOG.debug("   XmlSchemaElement RefName        = "
	    			+ obj.getRefName());
	    	LOG.debug("   XmlSchemaElement DefaultValue   = "
	    			+ obj.getDefaultValue());
	    	LOG.debug("   XmlSchemaElement FixedValue     = "
	    			+ obj.getFixedValue());
		}

    	/* Add cobol attributes valid for all types */
    	elc.setAttribute(CobolMarkup.LEVEL_NUMBER, Integer.toString(level));
    	elc.setAttribute(CobolMarkup.COBOL_NAME, getCobolName(obj.getName()));
    	
		if (LOG.isDebugEnabled()) {
	    	LOG.debug("   Cobol level          = "
	    			+ level);
	    	LOG.debug("   Cobol name           = "
	    			+ elc.getAttribute(CobolMarkup.COBOL_NAME));
		}
    	/* The semantic for maxOccurs is different for Cobol annotations than
    	 * for XML Schema. a maxOccurs of 1 is a one item array for Cobol which
    	 * is different from a simple item. If schema maxOccurs=1 we do not
    	 * insert a Cobol maxOccurs annotation at all. */
    	/* There is no natural mapping from XML schema arrays to Cobol arrays 
    	 * with depending on clause. This means that all XML Schema arrays are
    	 * mapped to fixed size Cobol arrays. Since this would result in very
    	 * inefficient Cobol structures, we impose a limit on arrays sizes. */
		if (obj.getMaxOccurs() > 1) {
	    	if (obj.getMaxOccurs() > Short.MAX_VALUE) {
	        	elc.setAttribute(CobolMarkup.MAX_OCCURS,
	        			XsdcUtil.getStringOption(mOptions,
	        					"default.max.occurs"));
		    	LOG.warn("Max occurs for element " + obj.getName()
		    			+ " has been set to " + Short.MAX_VALUE);
	    	} else {
	        	elc.setAttribute(CobolMarkup.MAX_OCCURS,
	        			Long.toString(obj.getMaxOccurs()));
	    	}
	    	
	    	elc.setAttribute(CobolMarkup.MIN_OCCURS,
	    			Long.toString(obj.getMinOccurs()));
	    	
			if (LOG.isDebugEnabled()) {
		    	LOG.debug("   Cobol minOccurs      = "
		    			+ elc.getAttribute(CobolMarkup.MIN_OCCURS));
		    	LOG.debug("   Cobol maxOccurs      = "
		    			+ elc.getAttribute(CobolMarkup.MAX_OCCURS));
			}
		}
		
		/* Examine inner simple and complex types */
    	if (obj.getSchemaType() instanceof XmlSchemaSimpleType) {
    		setSimpleTypeAttributes(schema,
    				(XmlSchemaSimpleType) obj.getSchemaType(), elc);
    	} else if (obj.getSchemaType() instanceof XmlSchemaComplexType) {
    		setComplexTypeAttributes(
    				schema, (XmlSchemaComplexType) obj.getSchemaType(), elc,
    				level);
    	}
		if (LOG.isDebugEnabled()) {
			LOG.debug("setAttributes ended for element = " + obj.getName());
		}
    }
    
    /**
     * Create a set of cobol attributes for a simple XML schema type.
     * @param schema the XML Schema being annotated
     * @param type the XML schema type
     * @param elc the DOM Element representing the Cobol annotation
     * @throws XsdCobolAnnotatorException if annotation fails
     */
    private void setSimpleTypeAttributes(
    		final XmlSchema schema,
    		final XmlSchemaSimpleType type,
    		final Element elc) throws XsdCobolAnnotatorException {

    	if (LOG.isDebugEnabled()) {
			LOG.debug("setSimpleTypeAttributes started for type = "
					+ type.getName());
	    	LOG.debug("   XmlSchemaType QName                   = "
	    			+ type.getQName());
	    	LOG.debug("   XmlSchemaType BaseSchemaType          = "
	    			+ type.getBaseSchemaType());
	    	LOG.debug("   XmlSchemaType DataType                = "
	    			+ type.getDataType());
	    	LOG.debug("   XmlSchemaType DeriveBy                = "
	    			+ type.getDeriveBy());
		}
    	/* Somewhere in this simple type hierarchy there must be a primitive
    	 * type from which it is derived. */
    	QName primitiveType = getPrimitiveType(schema, type);
    	
    	/* From the primitive XML schema type we infer a candidate Cobol type */
    	CobolType cobolType = mTypeMap.get(primitiveType);
    	if (cobolType == null) {
    		throw new XsdCobolAnnotatorException(
    				"Unsupported XML Schema type " + type.getQName());
    	}
    	elc.setAttribute(CobolMarkup.TYPE, cobolType.name());
		if (LOG.isDebugEnabled()) {
	    	LOG.debug("   Cobol type           = "
	    			+ elc.getAttribute(CobolMarkup.TYPE));
		}
		
		/* Simple types can derive from xsd:list, in which case we need to
		 * map them to arrays. Lists being unbounded, we need to artificially
		 * set a maximum bound to the corresponding Cobol array. */
    	if (type.getContent() instanceof XmlSchemaSimpleTypeList) {
        	elc.setAttribute(CobolMarkup.MIN_OCCURS, "1");
        	elc.setAttribute(CobolMarkup.MAX_OCCURS,
        			XsdcUtil.getStringOption(mOptions, "default.max.occurs"));
			if (LOG.isDebugEnabled()) {
		    	LOG.debug("   Cobol minOccurs      = "
		    			+ elc.getAttribute(CobolMarkup.MIN_OCCURS));
		    	LOG.debug("   Cobol maxOccurs      = "
		    			+ elc.getAttribute(CobolMarkup.MAX_OCCURS));
			}
    	}
    	
    	/* We collect all facets of interest from the type restriction */
    	XsdFacets facets = new XsdFacets();
    	getFacets(schema, type, facets);
    	
    	/* Based on the element type and facets we gather more attributes */
    	switch (cobolType) {
    	case ALPHANUMERIC_ITEM:
    		setAlphaNumericAttributes(primitiveType, facets, elc);
    		break;
    	case BINARY_ITEM:
    		setBinaryAttributes(primitiveType, facets, elc);
    		break;
    	case PACKED_DECIMAL_ITEM:
    		setDecimalAttributes(primitiveType, facets, elc);
    		break;
    	case SINGLE_FLOAT_ITEM:
    		setSingleFloatAttributes(primitiveType, facets, elc);
    		break;
    	case DOUBLE_FLOAT_ITEM:
    		setDoubleFloatAttributes(primitiveType, facets, elc);
    		break;
    	case OCTET_STREAM_ITEM:
    		setOctetStreamAttributes(primitiveType, facets, elc);
    		break;
    	default:
    		throw new XsdCobolAnnotatorException(
    				"Cobol type inferred is invalid");
    	}
		if (LOG.isDebugEnabled()) {
			LOG.debug("setSimpleTypeAttributes ended for type = "
					+ type.getName());
		}
    }
    
    /**
     * For each element child of a complex type, this method generates cobol
     * annotations.
     * @param schema the XML Schema being annotated
     * @param type the XML schema type
     * @param elc the DOM Element representing the Cobol annotation
     * @param level the current level in the type hierarchy
     * @throws XsdCobolAnnotatorException if annotation fails
     */
    private void setComplexTypeAttributes(
    		final XmlSchema schema,
    		final XmlSchemaComplexType type,
    		final Element elc,
    		final int level) throws XsdCobolAnnotatorException {
    	
    	if (LOG.isDebugEnabled()) {
			LOG.debug("setComplexTypeAttributes started for type = "
					+ type.getName());
	    	LOG.debug("   XmlSchemaType QName                    = "
	    			+ type.getQName());
		}
    	
    	elc.setAttribute(CobolMarkup.TYPE, CobolType.GROUP_ITEM.name());
		if (LOG.isDebugEnabled()) {
	    	LOG.debug("   Cobol type           = "
	    			+ elc.getAttribute(CobolMarkup.TYPE));
		}
		
		if (type.getParticle() instanceof XmlSchemaSequence) {
        	XmlSchemaSequence sequenceObj =
        		(XmlSchemaSequence) type.getParticle();
        	
        	if (sequenceObj.getMaxOccurs() > 1) {
                /* TODO find a way to handle occuring sequences */
        		LOG.warn("Complex type " + type.getName()
        			+ " contains a multi-occurence sequence that is ignored");
        	}
            XmlSchemaObjectCollection items = sequenceObj.getItems();
            /* Process each element in the collection */
            for (int i = 0; i < items.getCount(); i++) {
                XmlSchemaObject obj = items.getItem(i);
                if (obj instanceof XmlSchemaElement) {
                	annotateElement(schema, (XmlSchemaElement) obj, level + 2);
                }
            }
        } else {
            /* TODO process other particle types of interest */
        	LOG.warn("Complex type " + type.getName()
    				+ " does not contain a sequence");
        }
		if (LOG.isDebugEnabled()) {
			LOG.debug("setComplexTypeAttributes ended for type = "
					+ type.getName());
		}
    }
    
    /**
     * COBOL Alphanumerics are bounded. They must have a fixed size. This method
     * tries to infer one.
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setAlphaNumericAttributes(
    		final QName primitiveType,
    		final XsdFacets facets,
    		final Element elc) throws XsdCobolAnnotatorException {
    	
    	if (LOG.isDebugEnabled()) {
			LOG.debug("setAlphaNumericAttributes started for type = "
					+ primitiveType.getLocalPart());
		}
    	
    	/* If a byte length cannot be inferred from a facet in the XML schema*
    	 * type hierarchy, use the default. */
    	int byteLength = facets.getLength();
    	if (byteLength < 0) {
    		byteLength = XsdcUtil.getIntOption(mOptions,
    				"default.alphanumeric.len");
    	}
    	
    	/* TODO add analysis of pattern facet to refine type and picture 
    	 * inference 
    	 * TODO see if there is a way to set isJustifiedRight*/
     	elc.setAttribute(CobolMarkup.PICTURE, "X("
     			+ Integer.toString(byteLength) + ")");
     	elc.setAttribute(CobolMarkup.USAGE, "DISPLAY");
    	elc.setAttribute(CobolMarkup.BYTE_LENGTH, Integer.toString(byteLength));

    	if (LOG.isDebugEnabled()) {
			LOG.debug("setAlphaNumericAttributes ended for type = "
					+ primitiveType.getLocalPart());
	    	LOG.debug("   Cobol picture        = "
	    			+ elc.getAttribute(CobolMarkup.PICTURE));
	    	LOG.debug("   Cobol usage          = "
	    			+ elc.getAttribute(CobolMarkup.USAGE));
	    	LOG.debug("   Cobol byteLength     = "
	    			+ elc.getAttribute(CobolMarkup.BYTE_LENGTH));
		}
    }
    
    /**
     * COBOL octet stream data items are similar to alphanumerics.
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setOctetStreamAttributes(
    		final QName primitiveType,
    		final XsdFacets facets,
    		final Element elc) throws XsdCobolAnnotatorException {
    	
    	if (LOG.isDebugEnabled()) {
			LOG.debug("setOctetStreamAttributes started for type = "
					+ primitiveType.getLocalPart());
		}
    	/* If a byte length cannot be inferred from a facet in the XML schema*
    	 * type hierarchy, use the default. */
    	int byteLength = facets.getLength();
    	if (byteLength < 0) {
    		byteLength = XsdcUtil.getIntOption(mOptions,
    				"default.octet.stream.len");
    	}
    	
     	elc.setAttribute(CobolMarkup.PICTURE, "X("
     			+ Integer.toString(byteLength) + ")");
     	elc.setAttribute(CobolMarkup.USAGE, "DISPLAY");
    	elc.setAttribute(CobolMarkup.BYTE_LENGTH, Integer.toString(byteLength));

    	if (LOG.isDebugEnabled()) {
			LOG.debug("setOctetStreamAttributes ended for type = "
					+ primitiveType.getLocalPart());
	    	LOG.debug("   Cobol picture        = "
	    			+ elc.getAttribute(CobolMarkup.PICTURE));
	    	LOG.debug("   Cobol usage          = "
	    			+ elc.getAttribute(CobolMarkup.USAGE));
	    	LOG.debug("   Cobol byteLength     = "
	    			+ elc.getAttribute(CobolMarkup.BYTE_LENGTH));
		}
    }
    
    /**
     * COBOL Binary numerics are signed or unsigned and have a fixed number of
     * digits. This method infers a number of digits and a sign.
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setBinaryAttributes(
    		final QName primitiveType,
    		final XsdFacets facets,
    		final Element elc) throws XsdCobolAnnotatorException {
    	
    	if (LOG.isDebugEnabled()) {
			LOG.debug("setBinaryAttributes started for type = "
					+ primitiveType.getLocalPart());
		}
    	/* If total digits are not specified in the XML schema, infer a suitable
    	 *  default from the XML schema primitive type. */
    	int totalDigits = facets.getTotalDigits();
    	if (totalDigits < 0) {
    		totalDigits = XsdcUtil.getIntOption(mOptions,
    				"default.int.total.digits");
    		if (primitiveType.getLocalPart() == "boolean") {
    			totalDigits = XsdcUtil.getIntOption(mOptions, 
    					"default.bool.total.digits");
    		} else if (primitiveType.getLocalPart() == "unsignedShort") {
    			totalDigits = XsdcUtil.getIntOption(mOptions, 
    					"default.short.total.digits");
    		} else if (primitiveType.getLocalPart() == "unsignedLong") {
    			totalDigits = XsdcUtil.getIntOption(mOptions, 
    					"default.long.total.digits");
    		} else if (primitiveType.getLocalPart() == "long") {
    			totalDigits = XsdcUtil.getIntOption(mOptions, 
    					"default.long.total.digits");
    		} else if (primitiveType.getLocalPart() == "short") {
    			totalDigits = XsdcUtil.getIntOption(mOptions, 
    					"default.short.total.digits");
    		}
    		/* If a restriction on the number of digits is not explicitly
    		 * requested, use the cobol unlimited binary data type*/
         	elc.setAttribute(CobolMarkup.USAGE, "COMP-5");
    	} else {
         	elc.setAttribute(CobolMarkup.USAGE, "BINARY");
    	}
    	
    	/* Determine the byte length of the corresponding Cobol data item */
    	int byteLength;
    	if (totalDigits < 5) {
    		byteLength = 2;
    	} else if (totalDigits < 10) {
    		byteLength = 4;
    	} else if (totalDigits < 19) {
    		byteLength = 8;
    	} else {
    		throw new XsdCobolAnnotatorException(
    				"Cobol does not support numerics with more than 18 digits");
    	}
    	
    	/* Determine if this is an unsigned numeric */
    	boolean signed = true;
		if (primitiveType.getLocalPart() == "boolean"
			|| primitiveType.getLocalPart() == "positiveInteger"
			|| primitiveType.getLocalPart() == "nonNegativeInteger"
			|| primitiveType.getLocalPart() == "unsignedShort"
			|| primitiveType.getLocalPart() == "unsignedLong"
			|| primitiveType.getLocalPart() == "unsignedInt") {
			signed = false;
		}
    	
    	/* TODO add analysis of pattern facet to refine type and picture 
    	 * inference */
     	elc.setAttribute(CobolMarkup.PICTURE, "9("
     			+ Integer.toString(totalDigits) + ")");
     	elc.setAttribute(CobolMarkup.TOTAL_DIGITS,
     			Integer.toString(totalDigits));
    	elc.setAttribute(CobolMarkup.BYTE_LENGTH, Integer.toString(byteLength));
    	elc.setAttribute(CobolMarkup.IS_SIGNED, Boolean.toString(signed));

    	if (LOG.isDebugEnabled()) {
			LOG.debug("setBinaryAttributes ended for type = "
					+ primitiveType.getLocalPart());
	    	LOG.debug("   Cobol picture        = "
	    			+ elc.getAttribute(CobolMarkup.PICTURE));
	    	LOG.debug("   Cobol usage          = "
	    			+ elc.getAttribute(CobolMarkup.USAGE));
	    	LOG.debug("   Cobol byteLength     = "
	    			+ elc.getAttribute(CobolMarkup.BYTE_LENGTH));
	    	LOG.debug("   Cobol totalDigits    = "
	    			+ elc.getAttribute(CobolMarkup.TOTAL_DIGITS));
	    	LOG.debug("   Cobol isSigned       = "
	    			+ elc.getAttribute(CobolMarkup.IS_SIGNED));
		}
    }

    /**
     * COBOL Decimal numerics are signed or unsigned and have a fixed number of
     * total digits and fraction digits. This method infers numbers of digits
     * and sign.
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setDecimalAttributes(
    		final QName primitiveType,
    		final XsdFacets facets,
    		final Element elc) throws XsdCobolAnnotatorException {
    	
    	if (LOG.isDebugEnabled()) {
			LOG.debug("setDecimalAttributes started for type = "
					+ primitiveType.getLocalPart());
		}
    	
    	/* If digits numbers are not specified in the XML schema, infer a
    	 * suitable default from the XML schema primitive type. */
    	int totalDigits = facets.getTotalDigits();
    	if (totalDigits < 0) {
    		totalDigits = XsdcUtil.getIntOption(mOptions,
    				"default.dec.total.digits");
    	}
    	int fractionDigits = facets.getFractionDigits();
    	if (fractionDigits < 0) {
    		fractionDigits = XsdcUtil.getIntOption(mOptions,
    				"default.dec.frac.digits");
    	}
    	
    	/* Determine the byte length of the corresponding Cobol data item */
    	Double dbl =  Math.floor((totalDigits / 2) + 1);
    	int byteLength =  dbl.intValue();
    	
    	/* Consider decimals as always signed */
    	boolean signed = true;
    	
     	elc.setAttribute(CobolMarkup.PICTURE, "9("
     			+ Integer.toString(totalDigits - fractionDigits)
     			+ ((fractionDigits > 0)
     					? ")V9(" + Integer.toString(fractionDigits) : "")
     			+ ")");
     	elc.setAttribute(CobolMarkup.TOTAL_DIGITS,
     			Integer.toString(totalDigits));
     	elc.setAttribute(CobolMarkup.FRACTION_DIGITS,
     			Integer.toString(fractionDigits));
     	elc.setAttribute(CobolMarkup.USAGE, "COMP-3");
    	elc.setAttribute(CobolMarkup.BYTE_LENGTH, Integer.toString(byteLength));
    	elc.setAttribute(CobolMarkup.IS_SIGNED, Boolean.toString(signed));

    	if (LOG.isDebugEnabled()) {
			LOG.debug("setDecimalAttributes ended for type = "
					+ primitiveType.getLocalPart());
	    	LOG.debug("   Cobol picture        = "
	    			+ elc.getAttribute(CobolMarkup.PICTURE));
	    	LOG.debug("   Cobol usage          = "
	    			+ elc.getAttribute(CobolMarkup.USAGE));
	    	LOG.debug("   Cobol byteLength     = "
	    			+ elc.getAttribute(CobolMarkup.BYTE_LENGTH));
	    	LOG.debug("   Cobol totalDigits    = "
	    			+ elc.getAttribute(CobolMarkup.TOTAL_DIGITS));
	    	LOG.debug("   Cobol fractionDigits = "
	    			+ elc.getAttribute(CobolMarkup.FRACTION_DIGITS));
	    	LOG.debug("   Cobol isSigned       = "
	    			+ elc.getAttribute(CobolMarkup.IS_SIGNED));
		}
    }

    /**
     * COBOL single float numerics.
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setSingleFloatAttributes(
    		final QName primitiveType,
    		final XsdFacets facets,
    		final Element elc) throws XsdCobolAnnotatorException {
    	
    	if (LOG.isDebugEnabled()) {
			LOG.debug("setSingleFloatAttributes started for type = "
					+ primitiveType.getLocalPart());
		}
    	/* Determine the byte length of the corresponding Cobol data item */
    	int byteLength =  XsdcUtil.getIntOption(mOptions,
    			"single.float.byte.len");
    	
     	elc.setAttribute(CobolMarkup.USAGE, "COMP-1");
    	elc.setAttribute(CobolMarkup.BYTE_LENGTH, Integer.toString(byteLength));

    	if (LOG.isDebugEnabled()) {
			LOG.debug("setSingleFloatAttributes ended for type = "
					+ primitiveType.getLocalPart());
	    	LOG.debug("   Cobol usage          = "
	    			+ elc.getAttribute(CobolMarkup.USAGE));
	    	LOG.debug("   Cobol byteLength     = "
	    			+ elc.getAttribute(CobolMarkup.BYTE_LENGTH));
		}
    }

    /**
     * COBOL double float numerics.
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setDoubleFloatAttributes(
    		final QName primitiveType,
    		final XsdFacets facets,
    		final Element elc) throws XsdCobolAnnotatorException {
    	
    	if (LOG.isDebugEnabled()) {
			LOG.debug("setDoubleFloatAttributes started for type = "
					+ primitiveType.getLocalPart());
		}
    	/* Determine the byte length of the corresponding Cobol data item */
    	int byteLength =  XsdcUtil.getIntOption(mOptions,
    			"double.float.byte.len");
    	
     	elc.setAttribute(CobolMarkup.USAGE, "COMP-2");
    	elc.setAttribute(CobolMarkup.BYTE_LENGTH, Integer.toString(byteLength));

    	if (LOG.isDebugEnabled()) {
			LOG.debug("setDoubleFloatAttributes ended for type = "
					+ primitiveType.getLocalPart());
	    	LOG.debug("   Cobol usage          = "
	    			+ elc.getAttribute(CobolMarkup.USAGE));
	    	LOG.debug("   Cobol byteLength     = "
	    			+ elc.getAttribute(CobolMarkup.BYTE_LENGTH));
		}
    }

    /**
     * Inferring the XML schema primitive type involves a recursive search
     * because types can form a hierarchy and restrict each other.
     * @param schema the XML Schema being annotated
     * @param type the type from which a primitive type should be inferred
     * @return the primitive type
     * @throws XsdCobolAnnotatorException if primitive type cannot be inferred
     */
    private QName getPrimitiveType(
    		final XmlSchema schema,
    		final XmlSchemaSimpleType type) throws XsdCobolAnnotatorException {
    	
    	if (LOG.isDebugEnabled()) {
			LOG.debug("getPrimitiveType started for type = " + type.getName());
		}
    	
    	QName typeName = type.getQName();
    	if (typeName != null && typeName.getNamespaceURI() == XSD_NS) {
        	if (LOG.isDebugEnabled()) {
    			LOG.debug("getPrimitiveType ended for type = "
    					+ type.getName());
    			LOG.debug("   PrimitiveType = " + typeName);
    		}
    		return typeName;
    	}
    	if (type.getContent() != null) {
    		if (type.getContent() instanceof XmlSchemaSimpleTypeRestriction) {
    			XmlSchemaSimpleTypeRestriction restriction =
    				(XmlSchemaSimpleTypeRestriction) type.getContent();
    			/* For an unknown reason, getBaseType() sometimes returns null.
    			 * In such a case we have to locate the type using
    			 * getBaseTypeName()*/
    			if (restriction.getBaseType() == null) {
    				typeName = restriction.getBaseTypeName();
    				if (typeName != null) {
    			    	if (typeName.getNamespaceURI() == XSD_NS) {
    			        	if (LOG.isDebugEnabled()) {
    			    			LOG.debug("getPrimitiveType ended for type = "
    			    					+ type.getName());
    			    			LOG.debug("   PrimitiveType = " + typeName);
    			    		}
    			    		return typeName;
    			    	}
        				return getPrimitiveType(schema,
        						(XmlSchemaSimpleType) schema.getTypeByName(
        								typeName));
    				}
    			} else {
    				return getPrimitiveType(schema, restriction.getBaseType());
    			}
    		} else if (type.getContent() instanceof XmlSchemaSimpleTypeList) {
    			/* If this is a list, look for items type. */
				XmlSchemaSimpleTypeList listType =
					(XmlSchemaSimpleTypeList) type.getContent();
				return getPrimitiveType(schema, listType.getItemType());
    		}
    	}
		throw new XsdCobolAnnotatorException(
				"Cannot infer primitive type for " + typeName);
    }
    
    /**
     * Search for the all facets found in an XML schema type hierarchy. Since
     * we start from the most detailed type, the first facets encountered take
     * precedence over the ones we encounter higher in the hierarchy.
     * @param schema the XML Schema being annotated
     * @param type the type from which facets should be extracted
     * @param facets the facets extracted so far
     * @throws XsdCobolAnnotatorException if facets cannot be located
     */
    private void getFacets(
    		final XmlSchema schema,
    		final XmlSchemaSimpleType type,
    		final XsdFacets facets) throws XsdCobolAnnotatorException {
    	
    	/* facets are found in types restrictions */
    	if (type.getContent() == null) {
    		return;
    	}
    	if (LOG.isDebugEnabled()) {
			LOG.debug("getFacets started for type = " + type.getName());
		}
    	
		if (type.getContent() instanceof XmlSchemaSimpleTypeRestriction) {
			XmlSchemaSimpleTypeRestriction restriction =
				(XmlSchemaSimpleTypeRestriction) type.getContent();
			if (restriction.getFacets() != null) {
    			XmlSchemaObjectCollection collection = restriction.getFacets();
    	        for (Iterator i  = collection.getIterator(); i.hasNext();) {
    	        	XmlSchemaObject facet = (XmlSchemaObject) i.next();
    	        	/* When a facet value is found, we keep it only if
    	        	 * no previous type did set the same facet value */
    	        	if (facet instanceof XmlSchemaLengthFacet) {
    	        		XmlSchemaLengthFacet xsef =
    	        			(XmlSchemaLengthFacet) facet;
    	        		if (facets.getLength() == -1) {
    	        			facets.setLength(
    	        					new Integer((String) xsef.getValue()));
    	        		}
    	        	}
    	        	if (facet instanceof XmlSchemaPatternFacet) {
    	        		XmlSchemaPatternFacet xsef =
    	        			(XmlSchemaPatternFacet) facet;
    	        		if (facets.getPattern() == null) {
    	        			facets.setPattern((String) xsef.getValue());
    	        		}
    	        	}
    	        	if (facet instanceof XmlSchemaTotalDigitsFacet) {
    	        		XmlSchemaTotalDigitsFacet xsef =
    	        			(XmlSchemaTotalDigitsFacet) facet;
    	        		if (facets.getTotalDigits() == -1) {
    	        			facets.setTotalDigits(
    	        					new Integer((String) xsef.getValue()));
    	        		}
    	        	}
    	        	if (facet instanceof XmlSchemaFractionDigitsFacet) {
    	        		XmlSchemaFractionDigitsFacet xsef =
    	        			(XmlSchemaFractionDigitsFacet) facet;
    	        		if (facets.getFractionDigits() == -1) {
    	        			facets.setFractionDigits(
    	        					new Integer((String) xsef.getValue()));
    	        		}
    	        	}
    	        }
    		}
			
			/* If this type derives from another non-primitive one, continue the
			 * search up the hierarchy chain. */
			if (restriction.getBaseType() == null) {
				QName typeName = restriction.getBaseTypeName();
				if (typeName != null) {
			    	if (typeName.getNamespaceURI() == XSD_NS) {
			    		return;
			    	}
    				getFacets(schema,
    						(XmlSchemaSimpleType) schema.getTypeByName(
    								typeName), facets);
				}
			} else {
				getFacets(schema, restriction.getBaseType(), facets);
			}
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("getFacets ended for type = " + type.getName());
			LOG.debug("   Length facet         = " + facets.getLength());
			LOG.debug("   TotalDigits facet    = " + facets.getTotalDigits());
			LOG.debug("   FractionDigits facet = "
					+ facets.getFractionDigits());
			LOG.debug("   Pattern facet        = " + facets.getPattern());
		}
    }

    /** 
     * Method to infer a Cobol name from an XML schema type name.
     * @param xsdName the XSD type name
     * @return the proposed cobol name
     * @throws XsdCobolAnnotatorException if cobol name cannot be created
     * */
    public final String getCobolName(
    		final String xsdName) throws XsdCobolAnnotatorException {
    	try {
			return mNameResolver.getName(xsdName);
		} catch (CobolNameResolverException e) {
			throw new XsdCobolAnnotatorException(e);
		}
    }
    
	/**
	 * @return the input XML schema file
	 */
	public final File getInputXsdFile() {
		return mInputXsdFile;
	}

	/**
	 * @param xsdFile the input XML schema file to set
	 */
	public final void setInputXsdFile(
			final File xsdFile) {
		mInputXsdFile = xsdFile;
	}

	/**
	 * @return the current target directory
	 */
	public final File getTargetDir() {
		return mTargetDir;
	}

	/**
	 * @param targetDir the target directory to set
	 */
	public final void setTargetDir(final File targetDir) {
		mTargetDir = targetDir;
	}

	/**
	 * @return the target annotated XSD file name
	 */
	public final String getTargetXsdFileName() {
		return mTargetXsdFileName;
	}

	/**
	 * @param targetXsdFileName the target annotated XSD file name to set
	 */
	public final void setTargetXsdFileName(final String targetXsdFileName) {
		mTargetXsdFileName = targetXsdFileName;
	}

	/**
	 * @return the Suffix to be added to JAXB classes names for XML schema types
	 */
	public final String getJaxbTypeClassesSuffix() {
		return mJaxbTypeClassesSuffix;
	}

	/**
	 * @param jaxbTypeClassesSuffix the Suffix to be added to JAXB classes names
	 *  for XML schema types
	 */
	public final void setJaxbTypeClassesSuffix(
			final String jaxbTypeClassesSuffix) {
		mJaxbTypeClassesSuffix = jaxbTypeClassesSuffix;
	}

	/**
	 * @return the Jaxb Package Name
	 */
	public final String getJaxbPackageName() {
		return mJaxbPackageName;
	}

	/**
	 * @param jaxbPackageName the Jaxb Package Name to set
	 */
	public final void setJaxbPackageName(final String jaxbPackageName) {
		mJaxbPackageName = jaxbPackageName;
	}

}
