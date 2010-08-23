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
package com.legstar.xsdc.gen;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaFractionDigitsFacet;
import org.apache.ws.commons.schema.XmlSchemaImport;
import org.apache.ws.commons.schema.XmlSchemaLengthFacet;
import org.apache.ws.commons.schema.XmlSchemaObject;
import org.apache.ws.commons.schema.XmlSchemaObjectCollection;
import org.apache.ws.commons.schema.XmlSchemaPatternFacet;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.apache.ws.commons.schema.XmlSchemaSimpleType;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeList;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeRestriction;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeUnion;
import org.apache.ws.commons.schema.XmlSchemaTotalDigitsFacet;
import org.apache.ws.commons.schema.XmlSchemaType;
import org.apache.ws.commons.schema.utils.NamespaceMap;
import org.apache.ws.commons.schema.utils.NamespacePrefixList;
import org.w3c.dom.Attr;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.legstar.codegen.tasks.SourceToXsdCobolTask;
import com.legstar.coxb.CobolMarkup;
import com.legstar.coxb.CobolType;

/**
 * This Ant Task maps XML schema elements with Cobol data types. The result is
 * a new XML Schema with special annotations that can be used to map XML to
 * legacy data.
 *<p/>
 * TODO Examine processing of Default and Fixed attributes which are not handled
 * at the moment.
 */
public class XsdCobolAnnotator extends SourceToXsdCobolTask {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /* ====================================================================== */
    /* = Properties section = */
    /* ====================================================================== */

    /** This builder is used for annotation markup elements. */
    private final DocumentBuilder mDb;

    /** Associates a cobol type to an XSD primitive type. */
    private XsdCobolTypeMap mTypeMap = new XsdCobolTypeMap();

    /** Used to build valid cobol names from java names. */
    private CobolNameResolver mNameResolver;

    /** Parameters that can be externally modified. */
    private Properties mOptions;

    /**
     * Must be a map of Type names and Element names where Type names
     * must be registered.
     */
    private Map < QName, QName > mRootElements;

    /** Maps a complexType to a Java qualified class name. */
    private Map < String, String > mComplexTypeToJavaClassMap;

    /* ====================================================================== */
    /* = Constants section = */
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
    private static final String COBOL_NS = "http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd";

    /** Cobol annotations default prefix. */
    private static final String COBOL_PFX = "cb";

    /** Cobol annotation parent element name. */
    private static final String COBOL_PARENT_ELN = "cb:cobolElements";

    /** Cobol annotation element name. */
    private static final String COBOL_ELN = "cb:cobolElement";

    /** Cobol annotation parent complex type name. */
    private static final String COBOL_PARENT_CTN = "cb:cobolComplexTypes";

    /** Cobol annotation complex type name. */
    private static final String COBOL_CTN = "cb:cobolComplexType";

    /** Maximum size of a Cobol item name. */
    public static final int MAX_COBOLNAME_LEN = 30;

    /** The cobol reserved words substitution file name. */
    private static final String OPTIONS_FILE_NAME = "xsdcoptions.properties";

    /** This is the target namespace from the source Xsd. */
    private String mOriginalTargetNamespace;

    /** Indicates if namespace switching is requested. */
    private boolean mNeedNamespaceSwitch = false;

    /**
     * At construction time, a DOM factory is created. The DOM factory is later
     * used to create DOM documents which serve as home for all the annotation
     * markup nodes that will be created during the annotation process.
     * We also load cobol name substitution lists and options.
     */
    public XsdCobolAnnotator() {
        setModel(new XsdToXsdCobolModel());
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
     * The ant execute method. Generates a new annotated schema.
     */
    public void execute() {
        if (_log.isDebugEnabled()) {
            _log.debug("XML Schema Cobol annotation started");
        }
        checkInputXsd();
        XmlSchema schema = getSchema();

        checkAllParameters(schema);

        annotateSchema(schema);
        addRootElements(schema);

        OutputStream out = getOutputStream();

        XmlSchemaObjectCollection items = schema.getItems();

        /* Process each element in the input Schema */
        try {
            for (int i = 0; i < items.getCount(); i++) {
                XmlSchemaObject obj = items.getItem(i);
                if (obj instanceof XmlSchemaElement) {
                    annotateElement(schema, (XmlSchemaElement) obj, 1);
                }
                if (obj instanceof XmlSchemaComplexType) {
                    annotateComplexType(schema, (XmlSchemaComplexType) obj);
                }
            }
        } catch (XsdCobolAnnotatorException e) {
            throw (new BuildException(e));
        }

        /* Make sure the generated schema has an XML declaration */
        schema.setInputEncoding("UTF-8");
        Map < String, String > options = new HashMap < String, String >();
        options.put(OutputKeys.INDENT, "yes");
        options.put(OutputKeys.OMIT_XML_DECLARATION, "no");
        schema.write(out, options);
        if (_log.isDebugEnabled()) {
            _log.debug("XML Schema Cobol annotation ended");
        }
    }

    /**
     * As an option, caller might want to add root elements to the
     * schema before it is processed. To be valid these elements
     * must map to an existing type and should not conflict with
     * existing elements.
     * 
     * @param schema the schema to be annotated
     */
    private void addRootElements(final XmlSchema schema) {
        if (mRootElements == null) {
            return;
        }
        for (QName typeName : mRootElements.keySet()) {
            XmlSchemaType schemaType = schema.getTypeByName(typeName);
            QName eln = mRootElements.get(typeName);
            if (schema.getTypeByName(typeName) == null) {
                throw (new BuildException("Root element " + eln
                        + " has unknown type " + typeName));
            }

            QName elementName = mRootElements.get(typeName);
            XmlSchemaElement el = schema.getElementByName(elementName);
            if (el == null) {
                el = new XmlSchemaElement();
                el.setQName(elementName);
                el.setName(elementName.getLocalPart());
                el.setSchemaTypeName(typeName);
                el.setSchemaType(schemaType);
                schema.getElements().add(eln, el);
                schema.getItems().add(el);
            }
        }
    }

    /**
     * Checks that the xsd file provided is valid.
     */
    private void checkInputXsd() {

        if (_log.isDebugEnabled()) {
            _log.debug("   Source Xsd URI      = "
                    + ((getInputXsdUri() == null) ? null
                            : getInputXsdUri().toString()));
        }
        /* Check that we have a valid input XML schema. */
        if (getInputXsdUri() == null) {
            throw (new BuildException(
                    "Invalid input XML schema"));
        }

        /* Set a valid default target annotated XSD file name */
        if (getTargetXsdFileName() == null
                || getTargetXsdFileName().length() == 0) {
            String targetXsdFileName = getLastSegment(getInputXsdUri());
            /*
             * If there is no extension or extension is not xsd, add xsd as
             * the extension.
             */
            int p = targetXsdFileName.lastIndexOf('.');
            if (p > 0) {
                String ext = targetXsdFileName.substring(
                        p, targetXsdFileName.length());
                if (ext.compareToIgnoreCase(".xsd") != 0) {
                    targetXsdFileName += ".xsd";
                }
            } else {
                targetXsdFileName += ".xsd";
            }
            setTargetXsdFileName(targetXsdFileName);
        }
    }

    /**
     * Checks that parameters set are valid for the specified schema.
     * 
     * @param schema the input schema
     */
    private void checkAllParameters(final XmlSchema schema) {

        /* If the user did not specify a namespace get the schema one. */
        if (getNamespace() == null || getNamespace().length() == 0) {
            setNamespace(schema.getTargetNamespace());
        } else {
            /* Might be a case where we need to switch namespaces */
            switchTargetNamespace(schema, getNamespace());
        }

        /*
         * Xsd file name is not mandatory because we can generate a
         * sensible default value.
         */
        super.checkInput(false, false);

    }

    /**
     * Retrieves the last segment from a URI.
     * 
     * @param uri the uri to process
     * @return the last segment of the path
     */
    private String getLastSegment(final URI uri) {
        String path = uri.getPath();
        if (path == null || path.length() < 2) {
            return null;
        }
        if ((path.charAt(path.length() - 1)) == '/') {
            path = path.substring(0, path.length() - 1);
        }
        int pos = path.lastIndexOf('/');
        if (pos < 0) {
            return null;
        }
        return path.substring(++pos, path.length());
    }

    /**
     * Loads the input XML Schema either from an XSD file or a WSDL file.
     * 
     * @return a ws-commons XmlSchema instance.
     */
    @SuppressWarnings("unchecked")
    private XmlSchema getSchema() {

        if (_log.isDebugEnabled()) {
            _log.debug("getSchema started");
        }
        /* Load the input schema */
        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        XmlSchema schema;
        try {
            Document doc = mDb.parse(getInputXsdUri().toString());

            /* Get the root element (skipping any comments) */
            Node root = doc.getFirstChild();
            while (root != null && root instanceof Comment) {
                root = root.getNextSibling();
            }
            if (root == null) {
                throw new BuildException("File " + getInputXsdUri().toString()
                        + " does not contain an XML schema");
            }

            /* Look for an XML schema node */
            NodeList nodes = doc.getElementsByTagNameNS(XSD_NS, "schema");
            if (nodes == null || nodes.getLength() == 0) {
                throw new BuildException("File " + getInputXsdUri().toString()
                        + " does not contain an XML schema");
            }
            if (nodes.getLength() > 1) {
                _log.warn("Only the first XML schema in "
                        + getInputXsdUri().toString()
                        + " will be processed");
            }

            /* Translate that DOM schema node into an XmlSchema */
            schema = schemaCol.read((Element) nodes.item(0));

            /*
             * If this schema contains an include/import, then these includes
             * are what we need.
             * The previous schema element was a mere container.
             * Potentially, there might be more than one include/import but we
             * restrict ourselves
             * to the first one here.
             * TODO add support for multiple imports
             */
            XmlSchemaObjectCollection includes = schema.getIncludes();
            for (Iterator includedItems = includes.getIterator(); includedItems
                    .hasNext();) {
                Object include = includedItems.next();
                if (include instanceof XmlSchemaImport) {
                    schema = ((XmlSchemaImport) include).getSchema();
                    break;
                }
            }

            /*
             * In case this is a wsdl file, we store certain namespace
             * attributes of the root at the schema node level so that
             * the schema node is complete from an XmlSchema standpoint.
             * We filter out all WSDL related namespaces.
             */
            if (root.getLocalName().compareTo("definitions") == 0
                    && root.getNamespaceURI().compareTo(WSDL_NS) == 0) {

                NamespaceMap prefixmap = new NamespaceMap();
                NamespacePrefixList npl = schema.getNamespaceContext();
                for (int i = 0; i < npl.getDeclaredPrefixes().length; i++) {
                    prefixmap.add(npl.getDeclaredPrefixes()[i], npl
                            .getNamespaceURI(
                            npl.getDeclaredPrefixes()[i]));
                }
                for (int i = 0; i < root.getAttributes().getLength(); i++) {
                    Attr attribute = (Attr) root.getAttributes().item(i);
                    String namespaceURI = attribute.getNamespaceURI();
                    String value = attribute.getValue();
                    String name = attribute.getName();
                    if (name.equals("targetNamespace")
                            && schema.getTargetNamespace() == null) {
                        schema.setTargetNamespace(value);
                    } else if (namespaceURI != null
                            && namespaceURI.compareTo(NS_NS) == 0
                            && value.compareTo(SOAP_NS) != 0
                            && value.compareTo(WSDL_NS) != 0
                            && value.compareTo(ADDRESSING_NS) != 0) {
                        prefixmap.add(attribute.getLocalName(), value);
                    }
                }
                schema.setNamespaceContext(prefixmap);
            }

        } catch (FileNotFoundException e) {
            throw (new BuildException(e));
        } catch (SAXException e) {
            throw (new BuildException(e));
        } catch (IOException e) {
            throw (new BuildException(e));
        }

        if (_log.isDebugEnabled()) {
            _log.debug("getSchema ended. Target namespace = "
                    + schema.getTargetNamespace());
        }
        return schema;
    }

    /**
     * When the user requests a new target namespace, we need to switch
     * from the original one keeping the same prefix.
     * A namespace prefix list is immutable so we need to create a
     * complete namespace context.
     * 
     * @param schema the schema being built
     * @param newTargetNamespace the new target namespace
     */
    private void switchTargetNamespace(
            final XmlSchema schema, final String newTargetNamespace) {
        mOriginalTargetNamespace = schema.getTargetNamespace();
        if (mOriginalTargetNamespace.equals(newTargetNamespace)) {
            return;
        }
        mNeedNamespaceSwitch = true;
        schema.setTargetNamespace(newTargetNamespace);
        NamespaceMap prefixmap = new NamespaceMap();
        NamespacePrefixList npl = schema.getNamespaceContext();
        for (int i = 0; i < npl.getDeclaredPrefixes().length; i++) {
            String ns = npl.getNamespaceURI(npl.getDeclaredPrefixes()[i]);
            if (ns.equals(mOriginalTargetNamespace)) {
                prefixmap.add(npl.getDeclaredPrefixes()[i], newTargetNamespace);
            } else {
                prefixmap.add(npl.getDeclaredPrefixes()[i], ns);
            }
        }
        schema.setNamespaceContext(prefixmap);
    }

    /**
     * Switches namespace for a schema element.
     * 
     * @param schema the schema
     * @param obj the schema element
     */
    private void switchNamespace(
            final XmlSchema schema,
            final XmlSchemaElement obj) {
        if (obj.getQName().getNamespaceURI().equals(mOriginalTargetNamespace)) {
            QName newQName = new QName(
                    schema.getTargetNamespace(),
                    obj.getQName().getLocalPart(),
                    obj.getQName().getPrefix());
            obj.setQName(newQName);
        }
        QName typeQName = obj.getSchemaTypeName();
        if (typeQName.getNamespaceURI().equals(mOriginalTargetNamespace)) {
            QName newTypeQName = new QName(
                    schema.getTargetNamespace(),
                    typeQName.getLocalPart(),
                    typeQName.getPrefix());
            obj.setSchemaTypeName(newTypeQName);
        }
    }

    /**
     * Adds COXB annotations at the schema level.
     * 
     * @param schema the current schema being generated
     */
    private void annotateSchema(final XmlSchema schema) {
        if (_log.isDebugEnabled()) {
            _log.debug("AnnotateSchema started");
        }
        /* Add the JAXB and COXB namespaces to the target schema */
        NamespaceMap prefixmap = new NamespaceMap();
        NamespacePrefixList npl = schema.getNamespaceContext();
        for (int i = 0; i < npl.getDeclaredPrefixes().length; i++) {
            prefixmap.add(npl.getDeclaredPrefixes()[i], npl.getNamespaceURI(
                    npl.getDeclaredPrefixes()[i]));
        }
        /*
         * TODO check that prefix does not conflict with an existing one.
         * 3 situations might occur:
         * 1: an existing prefix with same value exists and points to a
         * different namespace
         * 2: namespace is already listed with a different prefix
         * 3: namespace is already listed with the same prefix
         */
        prefixmap.add(COBOL_PFX, COBOL_NS);
        schema.setNamespaceContext(prefixmap);

        if (_log.isDebugEnabled()) {
            _log.debug("AnnotateSchema ended");
        }

    }

    /**
     * Creates an output stream from the input XML schema file name.
     * 
     * @return an output stream
     */
    private OutputStream getOutputStream() {
        OutputStream out;
        String outPath = getTargetDir().getPath() + File.separator
                + getTargetXsdFileName();
        try {
            out = new FileOutputStream(new File(outPath));
            return out;
        } catch (FileNotFoundException e) {
            throw (new BuildException(e));
        }
    }

    /**
     * Main annotation process applied to an XML schema element.
     * 
     * @param schema the XML Schema being annotated
     * @param obj the XML Schema element to annotate
     * @param level the current level in the elements hierarchy. This is used
     *            to create Cobol levels with the same depth as the input XML
     *            schema.
     * @throws XsdCobolAnnotatorException if annotation fails
     */
    public void annotateElement(
            final XmlSchema schema,
            final XmlSchemaElement obj,
            final int level) throws XsdCobolAnnotatorException {
        /*
         * If this element is referencing another, it might not be useful to
         * annotate it.
         */
        if (obj.getRefName() != null) {
            return;
        }
        if (_log.isDebugEnabled()) {
            _log.debug("annotate started for element = " + obj.getName());
        }

        /* Create a DOM document to hold annotation notes */
        Document doc = mDb.newDocument();
        Element el = doc.createElementNS(COBOL_NS, COBOL_PARENT_ELN);
        Element elc = doc.createElementNS(COBOL_NS, COBOL_ELN);
        setAttributes(schema, obj, elc, level);
        el.appendChild(elc);

        /* Add an annotation */
        obj.setAnnotation(createAnnotation(el));

        if (mNeedNamespaceSwitch) {
            switchNamespace(schema, obj);
        }

        if (_log.isDebugEnabled()) {
            _log.debug("annotate ended for element = " + obj.getName());
        }
    }

    /**
     * Main annotation process applied to an XML schema complex type.
     * For now the only attribute that needs to go at the complex Type
     * level is the java class name used when the schema is derived from
     * a POJO rather than an XSD or WSDL.
     * 
     * @param schema the XML Schema being annotated
     * @param obj the XML Schema type to annotate
     * @throws XsdCobolAnnotatorException if annotation fails
     */
    public void annotateComplexType(
            final XmlSchema schema,
            final XmlSchemaComplexType obj) throws XsdCobolAnnotatorException {
        if (_log.isDebugEnabled()) {
            _log.debug("annotate started for complex type = " + obj.getName());
        }

        /*
         * If this complex type maps to a java class name, add this
         * attribute to the annotation
         */
        if (mComplexTypeToJavaClassMap != null) {
            String javaClassName =
                    mComplexTypeToJavaClassMap.get(obj.getName());
            if (javaClassName != null) {
                if (_log.isDebugEnabled()) {
                    _log.debug("   java class name = " + javaClassName);
                }
                /* Create a DOM document to hold annotation notes */
                Document doc = mDb.newDocument();
                Element el = doc.createElementNS(COBOL_NS, COBOL_PARENT_CTN);
                Element elc = doc.createElementNS(COBOL_NS, COBOL_CTN);
                elc.setAttribute(CobolMarkup.JAVA_CLASS_NAME, javaClassName);
                el.appendChild(elc);

                /* Add an annotation */
                obj.setAnnotation(createAnnotation(el));
            }
        }

        if (_log.isDebugEnabled()) {
            _log.debug("annotate ended for complex type = " + obj.getName());
        }
    }

    /**
     * Create an Xml schema annotation ready to be added on some schema
     * object.
     * 
     * @param el a DOM element holding the annotations as child elements
     * @return an Xml schema annotation
     */
    private XmlSchemaAnnotation createAnnotation(final Element el) {
        XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
        XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
        NodeList markup = el.getChildNodes();
        appInfo.setMarkup(markup);
        annotation.getItems().add(appInfo);
        return annotation;
    }

    /**
     * Create a set of attributes for the Cobol annotation element depending
     * on corresponding XML schema type.
     * 
     * @param schema the XML Schema being annotated
     * @param obj the XML Schema element to annotate
     * @param elc the DOM Element representing the Cobol annotation
     * @param level the current level in the elements hierarchy. This is used
     *            to create Cobol levels with the same depth as the input XML
     *            schema.
     * @throws XsdCobolAnnotatorException if annotation fails
     */
    private void setAttributes(
            final XmlSchema schema,
            final XmlSchemaElement obj,
            final Element elc,
            final int level) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("setAttributes started for element  = "
                    + obj.getName());
            _log.debug("   XmlSchemaElement QName          = "
                    + obj.getQName());
            _log.debug("   XmlSchemaElement SchemaType     = "
                    + obj.getSchemaType());
            _log.debug("   XmlSchemaElement SchemaTypeName = "
                    + obj.getSchemaTypeName());
            _log.debug("   XmlSchemaElement MaxOccurs      = "
                    + obj.getMaxOccurs());
            _log.debug("   XmlSchemaElement MinOccurs      = "
                    + obj.getMinOccurs());
            _log.debug("   XmlSchemaElement RefName        = "
                    + obj.getRefName());
            _log.debug("   XmlSchemaElement DefaultValue   = "
                    + obj.getDefaultValue());
            _log.debug("   XmlSchemaElement FixedValue     = "
                    + obj.getFixedValue());
        }

        /* Add cobol attributes valid for all types */
        elc.setAttribute(CobolMarkup.LEVEL_NUMBER, Integer.toString(level));
        elc.setAttribute(CobolMarkup.COBOL_NAME, getCobolName(obj.getName()));

        if (_log.isDebugEnabled()) {
            _log.debug("   Cobol level          = "
                    + level);
            _log.debug("   Cobol name           = "
                    + elc.getAttribute(CobolMarkup.COBOL_NAME));
        }
        /*
         * The semantic for maxOccurs is different for Cobol annotations than
         * for XML Schema. a maxOccurs of 1 is a one item array for Cobol which
         * is different from a simple item. If schema maxOccurs=1 we do not
         * insert a Cobol maxOccurs annotation at all.
         */
        /*
         * There is no natural mapping from XML schema arrays to Cobol arrays
         * with depending on clause. This means that all XML Schema arrays are
         * mapped to fixed size Cobol arrays. Since this would result in very
         * inefficient Cobol structures, we impose a limit on arrays sizes.
         */
        if (obj.getMaxOccurs() > 1) {
            if (obj.getMaxOccurs() > Short.MAX_VALUE) {
                String defaultMaxOccurs = XsdcUtil.getStringOption(mOptions,
                        "default.max.occurs");
                elc.setAttribute(CobolMarkup.MAX_OCCURS, defaultMaxOccurs);
                _log.warn("Max occurs for element " + obj.getName()
                        + " has been set to default value " + defaultMaxOccurs);
            } else {
                elc.setAttribute(CobolMarkup.MAX_OCCURS,
                        Long.toString(obj.getMaxOccurs()));
            }

            elc.setAttribute(CobolMarkup.MIN_OCCURS,
                    Long.toString(obj.getMinOccurs()));

            if (_log.isDebugEnabled()) {
                _log.debug("   Cobol minOccurs      = "
                        + elc.getAttribute(CobolMarkup.MIN_OCCURS));
                _log.debug("   Cobol maxOccurs      = "
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
        if (_log.isDebugEnabled()) {
            _log.debug("setAttributes ended for element = " + obj.getName());
        }
    }

    /**
     * Create a set of cobol attributes for a simple XML schema type.
     * 
     * @param schema the XML Schema being annotated
     * @param type the XML schema type
     * @param elc the DOM Element representing the Cobol annotation
     * @throws XsdCobolAnnotatorException if annotation fails
     */
    private void setSimpleTypeAttributes(
            final XmlSchema schema,
            final XmlSchemaSimpleType type,
            final Element elc) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("setSimpleTypeAttributes started for type = "
                    + type.getName());
            _log.debug("   XmlSchemaType QName                   = "
                    + type.getQName());
            _log.debug("   XmlSchemaType BaseSchemaType          = "
                    + type.getBaseSchemaType());
            _log.debug("   XmlSchemaType DataType                = "
                    + type.getDataType());
            _log.debug("   XmlSchemaType DeriveBy                = "
                    + type.getDeriveBy());
        }
        /*
         * Somewhere in this simple type hierarchy there must be a primitive
         * type from which it is derived.
         */
        QName primitiveType = getPrimitiveType(schema, type);

        /* From the primitive XML schema type we infer a candidate Cobol type */
        CobolType cobolType = mTypeMap.get(primitiveType);
        if (cobolType == null) {
            throw new XsdCobolAnnotatorException(
                    "Unsupported XML Schema type " + type.getQName());
        }
        elc.setAttribute(CobolMarkup.TYPE, cobolType.name());
        if (_log.isDebugEnabled()) {
            _log.debug("   Cobol type           = "
                    + elc.getAttribute(CobolMarkup.TYPE));
        }

        /*
         * Simple types can derive from xsd:list, in which case we need to
         * map them to arrays. Lists being unbounded, we need to artificially
         * set a maximum bound to the corresponding Cobol array.
         */
        if (type.getContent() instanceof XmlSchemaSimpleTypeList) {
            elc.setAttribute(CobolMarkup.MIN_OCCURS, "1");
            elc.setAttribute(CobolMarkup.MAX_OCCURS,
                    XsdcUtil.getStringOption(mOptions, "default.max.occurs"));
            if (_log.isDebugEnabled()) {
                _log.debug("   Cobol minOccurs      = "
                        + elc.getAttribute(CobolMarkup.MIN_OCCURS));
                _log.debug("   Cobol maxOccurs      = "
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
            setSingleFloatAttributes(primitiveType, elc);
            break;
        case DOUBLE_FLOAT_ITEM:
            setDoubleFloatAttributes(primitiveType, elc);
            break;
        case OCTET_STREAM_ITEM:
            setOctetStreamAttributes(primitiveType, facets, elc);
            break;
        default:
            throw new XsdCobolAnnotatorException(
                    "Cobol type inferred is invalid");
        }
        if (_log.isDebugEnabled()) {
            _log.debug("setSimpleTypeAttributes ended for type = "
                    + type.getName());
        }
    }

    /**
     * For each element child of a complex type, this method generates cobol
     * annotations.
     * 
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

        if (_log.isDebugEnabled()) {
            _log.debug("setComplexTypeAttributes started for type = "
                    + type.getName());
            _log.debug("   XmlSchemaType QName                    = "
                    + type.getQName());
        }

        elc.setAttribute(CobolMarkup.TYPE, CobolType.GROUP_ITEM.name());
        if (_log.isDebugEnabled()) {
            _log.debug("   Cobol type           = "
                    + elc.getAttribute(CobolMarkup.TYPE));
        }

        if (type.getParticle() instanceof XmlSchemaSequence) {
            XmlSchemaSequence sequenceObj =
                    (XmlSchemaSequence) type.getParticle();

            if (sequenceObj.getMaxOccurs() > 1) {
                /* TODO find a way to handle occuring sequences */
                _log
                        .warn("Complex type "
                                + type.getName()
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
            _log.warn("Complex type " + type.getName()
                    + " does not contain a sequence");
        }
        if (_log.isDebugEnabled()) {
            _log.debug("setComplexTypeAttributes ended for type = "
                    + type.getName());
        }
    }

    /**
     * COBOL Alphanumerics are bounded. They must have a fixed size. This method
     * tries to infer one.
     * 
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setAlphaNumericAttributes(
            final QName primitiveType,
            final XsdFacets facets,
            final Element elc) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("setAlphaNumericAttributes started for type = "
                    + primitiveType.getLocalPart());
        }

        /*
         * If a byte length cannot be inferred from a facet in the XML schema*
         * type hierarchy, use the default.
         */
        int byteLength = facets.getLength();
        if (byteLength < 0) {
            byteLength = XsdcUtil.getIntOption(mOptions,
                    "default.alphanumeric.len");
            _log.warn("Byte length for element "
                    + elc.getAttribute(CobolMarkup.COBOL_NAME)
                    + " has been set to default value " + byteLength);
        }

        /*
         * TODO add analysis of pattern facet to refine type and picture
         * inference
         * TODO see if there is a way to set isJustifiedRight
         */
        elc.setAttribute(CobolMarkup.PICTURE, "X("
                + Integer.toString(byteLength) + ")");
        elc.setAttribute(CobolMarkup.USAGE, "DISPLAY");

        if (_log.isDebugEnabled()) {
            _log.debug("setAlphaNumericAttributes ended for type = "
                    + primitiveType.getLocalPart());
            _log.debug("   Cobol picture        = "
                    + elc.getAttribute(CobolMarkup.PICTURE));
            _log.debug("   Cobol usage          = "
                    + elc.getAttribute(CobolMarkup.USAGE));
        }
    }

    /**
     * COBOL octet stream data items are similar to alphanumerics.
     * 
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setOctetStreamAttributes(
            final QName primitiveType,
            final XsdFacets facets,
            final Element elc) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("setOctetStreamAttributes started for type = "
                    + primitiveType.getLocalPart());
        }
        /*
         * If a byte length cannot be inferred from a facet in the XML schema*
         * type hierarchy, use the default.
         */
        int byteLength = facets.getLength();
        if (byteLength < 0) {
            byteLength = XsdcUtil.getIntOption(mOptions,
                    "default.octet.stream.len");
        }

        elc.setAttribute(CobolMarkup.PICTURE, "X("
                + Integer.toString(byteLength) + ")");
        elc.setAttribute(CobolMarkup.USAGE, "DISPLAY");

        if (_log.isDebugEnabled()) {
            _log.debug("setOctetStreamAttributes ended for type = "
                    + primitiveType.getLocalPart());
            _log.debug("   Cobol picture        = "
                    + elc.getAttribute(CobolMarkup.PICTURE));
            _log.debug("   Cobol usage          = "
                    + elc.getAttribute(CobolMarkup.USAGE));
        }
    }

    /**
     * COBOL Binary numerics are signed or unsigned and have a fixed number of
     * digits. This method infers a number of digits and a sign.
     * 
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setBinaryAttributes(
            final QName primitiveType,
            final XsdFacets facets,
            final Element elc) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("setBinaryAttributes started for type = "
                    + primitiveType.getLocalPart());
        }
        /*
         * If total digits are not specified in the XML schema, infer a suitable
         * default from the XML schema primitive type.
         */
        int totalDigits = facets.getTotalDigits();
        if (totalDigits < 0) {
            totalDigits = XsdcUtil.getIntOption(mOptions,
                    "default.int.total.digits");
            if (primitiveType.getLocalPart().equals("boolean")) {
                totalDigits = XsdcUtil.getIntOption(mOptions,
                        "default.bool.total.digits");
            } else if (primitiveType.getLocalPart().equals("unsignedShort")) {
                totalDigits = XsdcUtil.getIntOption(mOptions,
                        "default.short.total.digits");
            } else if (primitiveType.getLocalPart().equals("unsignedLong")) {
                totalDigits = XsdcUtil.getIntOption(mOptions,
                        "default.long.total.digits");
            } else if (primitiveType.getLocalPart().equals("long")) {
                totalDigits = XsdcUtil.getIntOption(mOptions,
                        "default.long.total.digits");
            } else if (primitiveType.getLocalPart().equals("short")) {
                totalDigits = XsdcUtil.getIntOption(mOptions,
                        "default.short.total.digits");
            }
            /*
             * If a restriction on the number of digits is not explicitly
             * requested, use the cobol unlimited binary data type
             */
            elc.setAttribute(CobolMarkup.USAGE, "COMP-5");
        } else {
            elc.setAttribute(CobolMarkup.USAGE, "BINARY");
        }

        /* Determine if this is an unsigned numeric */
        boolean signed = true;
        if (primitiveType.getLocalPart().equals("boolean")
                || primitiveType.getLocalPart().equals("positiveInteger")
                || primitiveType.getLocalPart().equals("nonNegativeInteger")
                || primitiveType.getLocalPart().equals("unsignedShort")
                || primitiveType.getLocalPart().equals("unsignedLong")
                || primitiveType.getLocalPart().equals("unsignedInt")) {
            signed = false;
        }

        /*
         * TODO add analysis of pattern facet to refine type and picture
         * inference
         */
        elc.setAttribute(CobolMarkup.PICTURE, "9("
                + Integer.toString(totalDigits) + ")");
        elc.setAttribute(CobolMarkup.TOTAL_DIGITS,
                Integer.toString(totalDigits));
        elc.setAttribute(CobolMarkup.IS_SIGNED, Boolean.toString(signed));

        if (_log.isDebugEnabled()) {
            _log.debug("setBinaryAttributes ended for type = "
                    + primitiveType.getLocalPart());
            _log.debug("   Cobol picture        = "
                    + elc.getAttribute(CobolMarkup.PICTURE));
            _log.debug("   Cobol usage          = "
                    + elc.getAttribute(CobolMarkup.USAGE));
            _log.debug("   Cobol totalDigits    = "
                    + elc.getAttribute(CobolMarkup.TOTAL_DIGITS));
            _log.debug("   Cobol isSigned       = "
                    + elc.getAttribute(CobolMarkup.IS_SIGNED));
        }
    }

    /**
     * COBOL Decimal numerics are signed or unsigned and have a fixed number of
     * total digits and fraction digits. This method infers numbers of digits
     * and sign.
     * 
     * @param primitiveType the XML Schema primitive type
     * @param facets the set of XML schema facets
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setDecimalAttributes(
            final QName primitiveType,
            final XsdFacets facets,
            final Element elc) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("setDecimalAttributes started for type = "
                    + primitiveType.getLocalPart());
        }

        /*
         * If digits numbers are not specified in the XML schema, infer a
         * suitable default from the XML schema primitive type.
         */
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
        elc.setAttribute(CobolMarkup.IS_SIGNED, Boolean.toString(signed));

        if (_log.isDebugEnabled()) {
            _log.debug("setDecimalAttributes ended for type = "
                    + primitiveType.getLocalPart());
            _log.debug("   Cobol picture        = "
                    + elc.getAttribute(CobolMarkup.PICTURE));
            _log.debug("   Cobol usage          = "
                    + elc.getAttribute(CobolMarkup.USAGE));
            _log.debug("   Cobol totalDigits    = "
                    + elc.getAttribute(CobolMarkup.TOTAL_DIGITS));
            _log.debug("   Cobol fractionDigits = "
                    + elc.getAttribute(CobolMarkup.FRACTION_DIGITS));
            _log.debug("   Cobol isSigned       = "
                    + elc.getAttribute(CobolMarkup.IS_SIGNED));
        }
    }

    /**
     * COBOL single float numerics.
     * 
     * @param primitiveType the XML Schema primitive type
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setSingleFloatAttributes(
            final QName primitiveType,
            final Element elc) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("setSingleFloatAttributes started for type = "
                    + primitiveType.getLocalPart());
        }

        elc.setAttribute(CobolMarkup.USAGE, "COMP-1");

        if (_log.isDebugEnabled()) {
            _log.debug("setSingleFloatAttributes ended for type = "
                    + primitiveType.getLocalPart());
            _log.debug("   Cobol usage          = "
                    + elc.getAttribute(CobolMarkup.USAGE));
        }
    }

    /**
     * COBOL double float numerics.
     * 
     * @param primitiveType the XML Schema primitive type
     * @param elc the annotated element
     * @throws XsdCobolAnnotatorException if attributes cannot be set
     */
    private void setDoubleFloatAttributes(
            final QName primitiveType,
            final Element elc) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("setDoubleFloatAttributes started for type = "
                    + primitiveType.getLocalPart());
        }

        elc.setAttribute(CobolMarkup.USAGE, "COMP-2");

        if (_log.isDebugEnabled()) {
            _log.debug("setDoubleFloatAttributes ended for type = "
                    + primitiveType.getLocalPart());
            _log.debug("   Cobol usage          = "
                    + elc.getAttribute(CobolMarkup.USAGE));
        }
    }

    /**
     * Inferring the XML schema primitive type involves a recursive search
     * because types can form a hierarchy and restrict each other.
     * 
     * @param schema the XML Schema being annotated
     * @param type the type from which a primitive type should be inferred
     * @return the primitive type
     * @throws XsdCobolAnnotatorException if primitive type cannot be inferred
     */
    private QName getPrimitiveType(
            final XmlSchema schema,
            final XmlSchemaSimpleType type) throws XsdCobolAnnotatorException {

        if (_log.isDebugEnabled()) {
            _log.debug("getPrimitiveType started for type = " + type.getName());
        }

        QName typeName = type.getQName();
        if (typeName != null && XSD_NS.equals(typeName.getNamespaceURI())) {
            if (_log.isDebugEnabled()) {
                _log.debug("getPrimitiveType ended for type = "
                        + type.getName());
                _log.debug("   PrimitiveType = " + typeName);
            }
            return typeName;
        }
        if (type.getContent() != null) {
            if (type.getContent() instanceof XmlSchemaSimpleTypeRestriction) {
                XmlSchemaSimpleTypeRestriction restriction =
                        (XmlSchemaSimpleTypeRestriction) type.getContent();
                /*
                 * For an unknown reason, getBaseType() sometimes returns null.
                 * In such a case we have to locate the type using
                 * getBaseTypeName()
                 */
                if (restriction.getBaseType() == null) {
                    typeName = restriction.getBaseTypeName();
                    if (typeName != null) {
                        if (XSD_NS.equals(typeName.getNamespaceURI())) {
                            if (_log.isDebugEnabled()) {
                                _log.debug("getPrimitiveType ended for type = "
                                        + type.getName());
                                _log.debug("   PrimitiveType = " + typeName);
                            }
                            return typeName;
                        }
                        /*
                         * Since restriction base type is not an XML Schema
                         * standard type, it must be defined in this schema.
                         * We don't support restrictions that are not simple
                         * types.
                         */
                        XmlSchemaType restrictionBaseType =
                                schema.getTypeByName(typeName);
                        if (restrictionBaseType != null
                                && restrictionBaseType
                                instanceof XmlSchemaSimpleType) {
                            return getPrimitiveType(schema,
                                    (XmlSchemaSimpleType) restrictionBaseType);
                        }
                    }
                } else {
                    return getPrimitiveType(schema, restriction.getBaseType());
                }

            } else if (type.getContent() instanceof XmlSchemaSimpleTypeList) {
                /* If this is a list, look for items type. */
                XmlSchemaSimpleTypeList listType =
                        (XmlSchemaSimpleTypeList) type.getContent();
                return getPrimitiveType(schema, listType.getItemType());

            } else if (type.getContent() instanceof XmlSchemaSimpleTypeUnion) {
                _log.warn(type.getName()
                        + " is a union. Processing first type in the union.");
                XmlSchemaSimpleTypeUnion simpleUnion = (XmlSchemaSimpleTypeUnion) type
                        .getContent();
                return getPrimitiveType(schema,
                        (XmlSchemaSimpleType) simpleUnion.getBaseTypes()
                                .getItem(0));
            }
        }
        throw new XsdCobolAnnotatorException(
                "Cannot infer primitive type for " + typeName);
    }

    /**
     * Search for the all facets found in an XML schema type hierarchy. Since
     * we start from the most detailed type, the first facets encountered take
     * precedence over the ones we encounter higher in the hierarchy.
     * 
     * @param schema the XML Schema being annotated
     * @param type the type from which facets should be extracted
     * @param facets the facets extracted so far
     * @throws XsdCobolAnnotatorException if facets cannot be located
     */
    @SuppressWarnings("unchecked")
    private void getFacets(
            final XmlSchema schema,
            final XmlSchemaSimpleType type,
            final XsdFacets facets) throws XsdCobolAnnotatorException {

        /* facets are found in types restrictions */
        if (type.getContent() == null) {
            return;
        }
        if (_log.isDebugEnabled()) {
            _log.debug("getFacets started for type = " + type.getName());
        }

        if (type.getContent() instanceof XmlSchemaSimpleTypeRestriction) {
            XmlSchemaSimpleTypeRestriction restriction =
                    (XmlSchemaSimpleTypeRestriction) type.getContent();
            if (restriction.getFacets() != null) {
                XmlSchemaObjectCollection collection = restriction.getFacets();
                for (Iterator < XmlSchemaObject > i =
                        collection.getIterator(); i.hasNext();) {
                    XmlSchemaObject facet = i.next();
                    /*
                     * When a facet value is found, we keep it only if
                     * no previous type did set the same facet value
                     */
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

            /*
             * If this type derives from another non-primitive one, continue the
             * search up the hierarchy chain.
             */
            if (restriction.getBaseType() == null) {
                QName typeName = restriction.getBaseTypeName();
                if (typeName != null) {
                    if (XSD_NS.equals(typeName.getNamespaceURI())) {
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

        if (_log.isDebugEnabled()) {
            _log.debug("getFacets ended for type = " + type.getName());
            _log.debug("   Length facet         = " + facets.getLength());
            _log.debug("   TotalDigits facet    = " + facets.getTotalDigits());
            _log.debug("   FractionDigits facet = "
                    + facets.getFractionDigits());
            _log.debug("   Pattern facet        = " + facets.getPattern());
        }
    }

    /**
     * Method to infer a Cobol name from an XML schema type name.
     * 
     * @param xsdName the XSD type name
     * @return the proposed cobol name
     * @throws XsdCobolAnnotatorException if cobol name cannot be created
     * */
    public String getCobolName(
            final String xsdName) throws XsdCobolAnnotatorException {
        try {
            return mNameResolver.getName(xsdName);
        } catch (CobolNameResolverException e) {
            throw new XsdCobolAnnotatorException(e);
        }
    }

    /**
     * @return the input XML schema uri
     */
    public URI getInputXsdUri() {
        return getModel().getInputXsdUri();
    }

    /**
     * @param xsdUri the input XML schema uri to set
     */
    public void setInputXsdUri(
            final URI xsdUri) {
        getModel().setInputXsdUri(xsdUri);
    }

    /**
     * @return the map of Type names / Element names for elements that need
     *         to be added to the annotated schema.
     */
    public Map < QName, QName > getRootElements() {
        return mRootElements;
    }

    /**
     * @param rootElements map of Type names / Element names for elements that
     *            need to be added to the annotated schema to set
     */
    public void setRootElements(final Map < QName, QName > rootElements) {
        mRootElements = rootElements;
    }

    /**
     * @return the complexType to Java qualified class name map
     */
    public Map < String, String > getComplexTypeToJavaClassMap() {
        return mComplexTypeToJavaClassMap;
    }

    /**
     * @param complexTypeToJavaClassMap the complexType to Java qualified class
     *            name map to set
     */
    public void setComplexTypeToJavaClassMap(
            final Map < String, String > complexTypeToJavaClassMap) {
        mComplexTypeToJavaClassMap = complexTypeToJavaClassMap;
    }

    /**
     * @return this model.
     */
    public XsdToXsdCobolModel getModel() {
        return (XsdToXsdCobolModel) super.getModel();
    }

}
