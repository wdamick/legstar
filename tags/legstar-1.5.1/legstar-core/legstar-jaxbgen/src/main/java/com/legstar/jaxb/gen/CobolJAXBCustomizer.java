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
package com.legstar.jaxb.gen;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.StringTokenizer;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAll;
import org.apache.ws.commons.schema.XmlSchemaAnnotated;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaEnumerationFacet;
import org.apache.ws.commons.schema.XmlSchemaGroup;
import org.apache.ws.commons.schema.XmlSchemaGroupRef;
import org.apache.ws.commons.schema.XmlSchemaObject;
import org.apache.ws.commons.schema.XmlSchemaObjectCollection;
import org.apache.ws.commons.schema.XmlSchemaParticle;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.apache.ws.commons.schema.XmlSchemaSimpleType;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeContent;
import org.apache.ws.commons.schema.XmlSchemaSimpleTypeRestriction;
import org.apache.ws.commons.schema.constants.Constants;
import org.apache.ws.commons.schema.utils.NamespaceMap;
import org.apache.ws.commons.schema.utils.NamespacePrefixList;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.legstar.coxb.CobolMarkup;
import com.legstar.coxb.util.NameUtil;

/**
 * This class implements the methods needed to inject custom JAXB bindings into
 * an XML Schema.
 * <p/>
 * The bindings are necessary so that XJC generates the correct JAXB Classes.
 * <p/>
 * This will not replace any customization that the user might have added
 * manually in the XML Schema.
 * 
 */
public class CobolJAXBCustomizer {

    /** Default XML schema character set. */
    private static final String DEFAULT_XSD_CHARSET = "UTF-8";

    /** Default prefix to prepend to enumration values. */
    private static final String DEFAULT_ENUMERATION_MEMBER_PREFIX = "VALUE_";

    /* ====================================================================== */
    /* Following are XML identifiers for JAXB annotations. = */
    /* ====================================================================== */

    /** The JAXB namespace. */
    private static final String JAXB_NAMESPACE = "http://java.sun.com/xml/ns/jaxb";

    /** The default prefix for the JAXB namespace. */
    private static final String JAXB_DEFAULT_NAMESPACE_PREFIX = "jaxb";

    /** The default prefix for the COBOL namespace. */
    private static final String COXB_DEFAULT_NAMESPACE_PREFIX = "cb";

    /** The JAXB version attribute name. */
    private static final String JAXB_VERSION_ATTR_NAME = "version";

    /** The JAXB version attribute value. */
    private static final String JAXB_VERSION_ATTR_VALUE = "2.0";

    /** Extension prefixes declared to JAXB. */
    private static final String JAXB_EXTENSION_BINDING_PREFIXES_ATTR_NAME = "extensionBindingPrefixes";

    /** JAXB annotation for global binding parameters. */
    private static final String JAXB_GLOBALBINDINGS = "globalBindings";

    /** JAXB annotation for generate isset method parameters. */
    private static final String JAXB_GENERATEISSETMETHOD = "generateIsSetMethod";

    /** JAXB annotation for serialization parameters. */
    private static final String JAXB_SERIALIZABLE = "serializable";

    /** JAXB annotation for serialization unique ID. */
    private static final String JAXB_UID = "uid";

    /** JAXB annotation for schema binding parameters. */
    private static final String JAXB_SCHEMABINDINGS = "schemaBindings";

    /** JAXB annotation for name transformation. */
    private static final String JAXB_XMLTRANSFORM = "nameXmlTransform";

    /** JAXB annotation for a type name transformation. */
    private static final String JAXB_TYPENAME = "typeName";

    /** JAXB annotation for an element name transformation. */
    private static final String JAXB_ELEMENTNAME = "elementName";

    /** JAXB annotation for a type name prefix transformation. */
    private static final String JAXB_PREFIX = "prefix";

    /** JAXB annotation for a type name suffix transformation. */
    private static final String JAXB_SUFFIX = "suffix";

    /** JAXB annotation for collection type. */
    private static final String JAXB_COLLECTION_TYPE = "collectionType";

    /** JAXB annotation for type safe Enum classes. */
    private static final String JAXB_TYPESAFEENUMCLASS = "typesafeEnumClass";

    /** JAXB annotation for type safe Enum class name attribute. */
    private static final String JAXB_TYPESAFEENUMCLASS_NAME = "name";

    /** JAXB annotation for type safe Enum members. */
    private static final String JAXB_TYPESAFEENUMMEMBER = "typesafeEnumMember";

    /** JAXB annotation for type safe Enum member name attribute. */
    private static final String JAXB_TYPESAFEENUMMEMBER_NAME = "name";

    /* ====================================================================== */
    /* Following private fields. = */
    /* ====================================================================== */

    /** This builder is used for annotation markup elements. */
    private final DocumentBuilder _db;

    /** The bindings parameters. */
    private JaxbGenModel _jaxbGenModel;

    /** Logging. */
    private Log _log = LogFactory.getLog(getClass());

    /**
     * Construct the customizer.
     * 
     * @param xjbModel the bindings parameters
     * @throws ParserConfigurationException if can't configure a DOM parser
     */
    public CobolJAXBCustomizer(final JaxbGenModel xjbModel)
            throws ParserConfigurationException {
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        _db = docFac.newDocumentBuilder();
        _jaxbGenModel = xjbModel;
    }

    /**
     * Adds JAXB custom bindings to an XML Schema.
     * 
     * @param sourceXsdFile the source XML Schema file
     * @param targetXsdFile the target, customized, XML Schema file
     * @throws IOException if output file cannot be written
     */
    public void customize(final File sourceXsdFile, final File targetXsdFile)
            throws IOException {
        customize(sourceXsdFile, DEFAULT_XSD_CHARSET, targetXsdFile,
                DEFAULT_XSD_CHARSET);

    }

    /**
     * Adds JAXB custom bindings to an XML Schema.
     * 
     * @param sourceXsdFile the source XML Schema file
     * @param sourceXsdCharset the source character set
     * @param targetXsdFile the target, customized, XML Schema file
     * @param targetXsdCharset the target character set
     * @throws IOException if output file cannot be written
     */
    public void customize(final File sourceXsdFile,
            final String sourceXsdCharset, final File targetXsdFile,
            final String targetXsdCharset) throws IOException {
        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        XmlSchema schema = schemaCol.read(new InputStreamReader(
                new FileInputStream(sourceXsdFile), sourceXsdCharset), null);

        String jaxbPrefix = injectJaxbNamespace(schema);

        injectJaxbExtensionAttributes(schema, jaxbPrefix);
        injectJaxbSchemaAnnotations(schema, JAXB_NAMESPACE, jaxbPrefix);
        injectJaxbEnumerationsAnnotation(schema, JAXB_NAMESPACE, jaxbPrefix);

        Writer writer = new OutputStreamWriter(new FileOutputStream(
                targetXsdFile), targetXsdCharset);
        schema.write(writer);
        writer.close();
    }

    /**
     * Searches the schema element for the JAXB namespace. If the namespace is
     * found, the associated prefix is returned otherwise, the JAXB namespace is
     * added to the schema.
     * 
     * @param xsd the XML Schema
     * @return the JAXB namespace prefix
     */
    public String injectJaxbNamespace(final XmlSchema xsd) {
        boolean hasJAXBNamespace = false;
        String jaxbPrefix = JAXB_DEFAULT_NAMESPACE_PREFIX;

        NamespaceMap prefixMap = new NamespaceMap();
        NamespacePrefixList nsList = xsd.getNamespaceContext();
        for (String prefix : nsList.getDeclaredPrefixes()) {
            if (nsList.getNamespaceURI(prefix).equals(JAXB_NAMESPACE)) {
                hasJAXBNamespace = true;
                jaxbPrefix = prefix;
                break;
            }
            prefixMap.add(prefix, nsList.getNamespaceURI(prefix));
        }

        if (!hasJAXBNamespace) {
            int incr = 0;
            while (prefixMap.containsKey(jaxbPrefix)) {
                incr++;
                jaxbPrefix = jaxbPrefix + Integer.toString(incr);
            }
            prefixMap.add(jaxbPrefix, JAXB_NAMESPACE);
            xsd.setNamespaceContext(prefixMap);
        }

        return jaxbPrefix;
    }

    /**
     * JAXB needs to know the LegStar extension prefix used. Here we lookup the
     * extension attribute and version if they are found, we add to them
     * otherwise we create new attributes.
     * <p/>
     * 
     * @param xsd the XML Schema
     * @param jaxbPrefix the JAXB namespace prefix
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public void injectJaxbExtensionAttributes(final XmlSchema xsd,
            final String jaxbPrefix) {

        // Lookup the LegStar namespace prefix
        String coxbPrefix = COXB_DEFAULT_NAMESPACE_PREFIX;
        NamespacePrefixList nsList = xsd.getNamespaceContext();
        for (String prefix : nsList.getDeclaredPrefixes()) {
            if (nsList.getNamespaceURI(prefix).equals(CobolMarkup.NS)) {
                coxbPrefix = prefix;
                break;
            }
        }

        // Retrieve extension attributes if any
        Map metaInfoMap = xsd.getMetaInfoMap();
        Map < QName, Attr > extensionMap = null;
        if (metaInfoMap != null) {
            extensionMap = (Map < QName, Attr >) metaInfoMap
                    .get(Constants.MetaDataConstants.EXTERNAL_ATTRIBUTES);
        } else {
            metaInfoMap = new LinkedHashMap();
            xsd.setMetaInfoMap(metaInfoMap);
        }
        if (extensionMap == null) {
            extensionMap = new HashMap < QName, Attr >();
        }

        // Extension attributes are DOM attributes
        Document doc = _db.newDocument();

        // Make sure the JAXB version extension is added
        QName versionQName = new QName(JAXB_NAMESPACE, JAXB_VERSION_ATTR_NAME);
        Attr attrib = doc.createAttribute(jaxbPrefix + ':'
                + JAXB_VERSION_ATTR_NAME);
        attrib.setValue(JAXB_VERSION_ATTR_VALUE);
        extensionMap.put(versionQName, attrib);

        /*
         * JAXB extension prefixes might already be present in which case we
         * make sure the legstar extension is there too. Extension prefixes are
         * specified as a whitespace-separated list of namespace prefixes.
         */

        QName extpfxQName = new QName(JAXB_NAMESPACE,
                JAXB_EXTENSION_BINDING_PREFIXES_ATTR_NAME);
        attrib = extensionMap.get(extpfxQName);
        if (attrib == null) {
            attrib = doc.createAttribute(jaxbPrefix + ':'
                    + JAXB_EXTENSION_BINDING_PREFIXES_ATTR_NAME);
        }
        String extpfx = attrib.getValue();
        if (extpfx == null || extpfx.length() == 0) {
            extpfx = coxbPrefix;
        } else {
            boolean hasCoxbPrefix = false;
            StringTokenizer tokenizer = new StringTokenizer(extpfx, " ");
            while (tokenizer.hasMoreTokens()) {
                if (tokenizer.nextToken().equals(coxbPrefix)) {
                    hasCoxbPrefix = true;
                    break;
                }
            }
            if (!hasCoxbPrefix) {
                extpfx = extpfx + " " + coxbPrefix;
            }
        }
        attrib.setValue(extpfx);
        extensionMap.put(extpfxQName, attrib);

        metaInfoMap.put(Constants.MetaDataConstants.EXTERNAL_ATTRIBUTES,
                extensionMap);
    }

    /**
     * Given an XML Schema, this will inject or replace custom JAXB bindings
     * annotations at the schema level.
     * <p/>
     * The generated schema holds JAXB annotations needed when, later on, the
     * schema is used to generate JAXB classes. The markup produced looks like
     * this:
     * 
     * <pre>
     * &lt;xsd:appinfo>
     *    &lt;jaxb:globalBindings generateIsSetMethod="true">
     *       &lt;jxb:serializable uid="1"/>
     *    &lt;/jaxb:globalBindings>
     *    &lt;jaxb:schemaBindings>
     *       &lt;jaxb:nameXmlTransform>
     *          &lt;jaxb:typeName prefix="Type" suffix="Type" />
     *          &lt;jaxb:elementName prefix="Type" suffix="Type" />
     *        &lt;/jaxb:nameXmlTransform>
     *    &lt;/jaxb:schemaBindings>
     * &lt;/xsd:appinfo>
     * </pre>
     * 
     * @param schema the XML Schema
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     */
    public void injectJaxbSchemaAnnotations(final XmlSchema schema,
            final String jaxbNamespace, final String jaxbNamespacePrefix) {

        XmlSchemaAppInfo appInfo = getXmlSchemaAppInfo(schema);
        DocumentFragment markupParent = getMarkupParent(appInfo);

        injectJaxbGlobalBindingsAnnotations(markupParent, jaxbNamespace,
                jaxbNamespacePrefix);

        injectJaxbSchemaBindingsAnnotations(markupParent, jaxbNamespace,
                jaxbNamespacePrefix);

        appInfo.setMarkup(markupParent.getChildNodes());

    }

    /**
     * Try to recover application info from an XML schema element and create new
     * ones if none is found.
     * 
     * @param element an XML schema element that might hold annotations
     * @return an existing or new XML schema application info
     */
    @SuppressWarnings("unchecked")
    protected XmlSchemaAppInfo getXmlSchemaAppInfo(
            final XmlSchemaAnnotated element) {

        XmlSchemaAnnotation annotation = null;
        if (element.getAnnotation() == null) {
            annotation = new XmlSchemaAnnotation();
        } else {
            annotation = element.getAnnotation();
        }

        /* Annotation might already contain an appinfo */
        XmlSchemaAppInfo appInfo = null;
        for (Iterator < XmlSchemaObject > i = annotation.getItems()
                .getIterator(); i.hasNext();) {
            XmlSchemaObject subAnnotation = i.next();
            if (subAnnotation instanceof XmlSchemaAppInfo) {
                appInfo = (XmlSchemaAppInfo) subAnnotation;
                break;
            }
        }
        if (appInfo == null) {
            appInfo = new XmlSchemaAppInfo();
            annotation.getItems().add(appInfo);
        }

        element.setAnnotation(annotation);
        return appInfo;
    }

    /**
     * The content of appinfo is a raw W3C nodelist. It is convenient to put
     * this nodelist inside a DOM fragment called parent markup that we create
     * or recover here.
     * 
     * @param appInfo the application info element
     * @return a parent DOM fragment for the annotation nodes list
     */
    protected DocumentFragment getMarkupParent(final XmlSchemaAppInfo appInfo) {

        NodeList markup = appInfo.getMarkup();
        DocumentFragment markupParent = null;
        if (markup == null || markup.getLength() == 0) {
            markupParent = _db.newDocument().createDocumentFragment();
        } else {
            markupParent = (DocumentFragment) markup.item(0).getParentNode();
        }
        return markupParent;
    }

    /**
     * Inject a global bindings element in the parent annotation node.
     * <p/>
     * If the element is already present we update its attributes.
     * 
     * @param markupParent the parent annotation node (its a document fragment)
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     */
    public void injectJaxbGlobalBindingsAnnotations(
            final DocumentFragment markupParent, final String jaxbNamespace,
            final String jaxbNamespacePrefix) {

        Element globalbindingsEl = getElement(markupParent, jaxbNamespace,
                jaxbNamespacePrefix, JAXB_GLOBALBINDINGS);
        globalbindingsEl.setAttribute(JAXB_GENERATEISSETMETHOD,
                Boolean.toString(getJaxbGenModel().isGenerateIsSetMethod()));

        /* ECI expects indexed collections not Lists (the default JAXB) */
        if (getJaxbGenModel().isEciCompatible()) {
            globalbindingsEl.setAttribute(JAXB_COLLECTION_TYPE, "indexed");
        }

        injectJaxbSerializableAnnotation(globalbindingsEl, jaxbNamespace,
                jaxbNamespacePrefix);

    }

    /**
     * Lookup a DOM element in the parent markup. If not found, an new DOM
     * element is created and added to the parent markup.
     * 
     * @param markupParent the parent markup
     * @param namespace the DOM namespace
     * @param namespacePrefix the DOM namespace prefix
     * @param elementLocalName the element local name
     * @return an existing or new DOM element
     */
    protected Element getElement(final DocumentFragment markupParent,
            final String namespace, final String namespacePrefix,
            final String elementLocalName) {

        Element domElement = null;
        NodeList nl = markupParent.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            if (nl.item(i) instanceof Element
                    && nl.item(i).getNamespaceURI().equals(namespace)
                    && nl.item(i).getLocalName().equals(elementLocalName)) {
                domElement = (Element) nl.item(i);
                break;
            }
        }
        if (domElement == null) {
            domElement = markupParent.getOwnerDocument().createElementNS(
                    namespace, namespacePrefix + ':' + elementLocalName);
            markupParent.appendChild(domElement);
        }
        return domElement;
    }

    /**
     * Inject a serializable element in the JAXB global bindings annotation.
     * <p/>
     * If the element is already present we update its attributes.
     * 
     * @param globalbindingsEl the global bindings node
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     */
    public void injectJaxbSerializableAnnotation(
            final Element globalbindingsEl, final String jaxbNamespace,
            final String jaxbNamespacePrefix) {

        Element serializableEl = null;
        NodeList nl = globalbindingsEl.getElementsByTagNameNS(jaxbNamespace,
                JAXB_SERIALIZABLE);
        if (nl.getLength() > 0) {
            serializableEl = (Element) nl.item(0);
        } else {
            serializableEl = globalbindingsEl.getOwnerDocument()
                    .createElementNS(jaxbNamespace,
                            jaxbNamespacePrefix + ':' + JAXB_SERIALIZABLE);
            globalbindingsEl.appendChild(serializableEl);
        }
        serializableEl.setAttribute(JAXB_UID,
                Long.toString(getJaxbGenModel().getSerializableUid()));
    }

    /**
     * Inject a schema bindings element in the parent annotation node.
     * <p/>
     * If the element is already present we update its attributes.
     * 
     * @param markupParent the parent annotation node (its a document fragment)
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     */
    public void injectJaxbSchemaBindingsAnnotations(
            final DocumentFragment markupParent, final String jaxbNamespace,
            final String jaxbNamespacePrefix) {

        Element schemabindingsEl = null;
        NodeList nl = markupParent.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            if (nl.item(i) instanceof Element
                    && nl.item(i).getNamespaceURI().equals(jaxbNamespace)
                    && nl.item(i).getLocalName().equals(JAXB_SCHEMABINDINGS)) {
                schemabindingsEl = (Element) nl.item(i);
                break;
            }
        }
        if (schemabindingsEl == null) {
            schemabindingsEl = markupParent.getOwnerDocument().createElementNS(
                    jaxbNamespace,
                    jaxbNamespacePrefix + ':' + JAXB_SCHEMABINDINGS);
            markupParent.appendChild(schemabindingsEl);
        }

        if (getJaxbGenModel().needXmlTransform()) {
            injectJaxbXmlTransformAnnotation(schemabindingsEl, jaxbNamespace,
                    jaxbNamespacePrefix);
        }

    }

    /**
     * Inject a XmlTransform element in the JAXB schema bindings annotation.
     * <p/>
     * If the element is already present we update its attributes.
     * 
     * @param schemabindingsEl the schema bindings node
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     */
    public void injectJaxbXmlTransformAnnotation(
            final Element schemabindingsEl, final String jaxbNamespace,
            final String jaxbNamespacePrefix) {

        Element xmltransformEl = null;
        NodeList nl = schemabindingsEl.getElementsByTagNameNS(jaxbNamespace,
                JAXB_XMLTRANSFORM);
        if (nl.getLength() > 0) {
            xmltransformEl = (Element) nl.item(0);
        } else {
            xmltransformEl = schemabindingsEl.getOwnerDocument()
                    .createElementNS(jaxbNamespace,
                            jaxbNamespacePrefix + ':' + JAXB_XMLTRANSFORM);
            schemabindingsEl.appendChild(xmltransformEl);
        }
        if (getJaxbGenModel().needTypeNameXmlTransform()) {
            injectJaxbTypeNameXmlTransformAnnotation(xmltransformEl,
                    jaxbNamespace, jaxbNamespacePrefix);
        }
        if (getJaxbGenModel().needElementNameXmlTransform()) {
            injectJaxbElementNameXmlTransformAnnotation(xmltransformEl,
                    jaxbNamespace, jaxbNamespacePrefix);
        }
    }

    /**
     * Inject a TypeNameXmlTransform element in the JAXB XmlTransform
     * annotation.
     * <p/>
     * If the element is already present we update its attributes.
     * 
     * @param xmltransformEl the XmlTransform node
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     */
    public void injectJaxbTypeNameXmlTransformAnnotation(
            final Element xmltransformEl, final String jaxbNamespace,
            final String jaxbNamespacePrefix) {

        Element typenamexmltransformEl = null;
        NodeList nl = xmltransformEl.getElementsByTagNameNS(jaxbNamespace,
                JAXB_TYPENAME);
        if (nl.getLength() > 0) {
            typenamexmltransformEl = (Element) nl.item(0);
        } else {
            typenamexmltransformEl = xmltransformEl.getOwnerDocument()
                    .createElementNS(jaxbNamespace,
                            jaxbNamespacePrefix + ':' + JAXB_TYPENAME);
            xmltransformEl.appendChild(typenamexmltransformEl);
        }
        if (getJaxbGenModel().getTypeNamePrefix() != null) {
            typenamexmltransformEl.setAttribute(JAXB_PREFIX, getJaxbGenModel()
                    .getTypeNamePrefix());
        }
        if (getJaxbGenModel().getTypeNameSuffix() != null) {
            typenamexmltransformEl.setAttribute(JAXB_SUFFIX, getJaxbGenModel()
                    .getTypeNameSuffix());
        }
    }

    /**
     * Inject a ElementNameXmlTransform element in the JAXB XmlTransform
     * annotation.
     * <p/>
     * If the element is already present we update its attributes.
     * 
     * @param xmltransformEl the XmlTransform node
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     */
    public void injectJaxbElementNameXmlTransformAnnotation(
            final Element xmltransformEl, final String jaxbNamespace,
            final String jaxbNamespacePrefix) {

        Element elementnamexmltransformEl = null;
        NodeList nl = xmltransformEl.getElementsByTagNameNS(jaxbNamespace,
                JAXB_ELEMENTNAME);
        if (nl.getLength() > 0) {
            elementnamexmltransformEl = (Element) nl.item(0);
        } else {
            elementnamexmltransformEl = xmltransformEl.getOwnerDocument()
                    .createElementNS(jaxbNamespace,
                            jaxbNamespacePrefix + ':' + JAXB_ELEMENTNAME);
            xmltransformEl.appendChild(elementnamexmltransformEl);
        }
        if (getJaxbGenModel().getElementNamePrefix() != null) {
            elementnamexmltransformEl.setAttribute(JAXB_PREFIX,
                    getJaxbGenModel().getElementNamePrefix());
        }
        if (getJaxbGenModel().getElementNameSuffix() != null) {
            elementnamexmltransformEl.setAttribute(JAXB_SUFFIX,
                    getJaxbGenModel().getElementNameSuffix());
        }
    }

    /**
     * Enumerations in anonymous simple types are not mapped to type safe Enum
     * classes by JAXB unless they are annotated.
     * <p/>
     * Since this is how cob2xsd handles COBOL level 88, we need these extra
     * annotations.
     * 
     * @param schema the XML Schema
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     */
    public void injectJaxbEnumerationsAnnotation(final XmlSchema schema,
            final String jaxbNamespace, final String jaxbNamespacePrefix) {

        if (_log.isDebugEnabled()) {
            _log.debug("injecting JAXB annotations on enumerations started");
        }
        processCollectionElements(schema, jaxbNamespace, jaxbNamespacePrefix,
                schema.getItems());

        if (_log.isDebugEnabled()) {
            _log.debug("injecting JAXB annotations on enumerations ended");
        }
    }

    /**
     * Take all elements from a collection and process them.
     * 
     * @param schema the XML Schema
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param items the parent collection
     */
    protected void processCollectionElements(final XmlSchema schema,
            final String jaxbNamespace, final String jaxbNamespacePrefix,
            final XmlSchemaObjectCollection items) {

        for (int i = 0; i < items.getCount(); i++) {
            XmlSchemaObject element = items.getItem(i);
            if (element instanceof XmlSchemaElement) {
                processElement(schema, jaxbNamespace, jaxbNamespacePrefix,
                        (XmlSchemaElement) element);
            } else if (element instanceof XmlSchemaGroupRef) {
                XmlSchemaGroupRef groupRef = (XmlSchemaGroupRef) element;
                XmlSchemaGroup group = (XmlSchemaGroup) schema.getGroups()
                        .getItem(groupRef.getRefName());
                processParticle(schema, jaxbNamespace, jaxbNamespacePrefix,
                        group.getName(), group.getParticle());
            }

        }
    }

    /**
     * Process an XML schema element.
     * 
     * @param schema the XML Schema
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param xsdElement the XML Schema element to process
     */
    protected void processElement(final XmlSchema schema,
            final String jaxbNamespace, final String jaxbNamespacePrefix,
            final XmlSchemaElement xsdElement) {
        if (xsdElement.getRefName() != null) {
            return;
        }
        if (_log.isDebugEnabled()) {
            _log.debug("process started for element = " + xsdElement.getName());
        }

        if (xsdElement.getSchemaType() instanceof XmlSchemaComplexType) {
            XmlSchemaComplexType xsdComplexType = (XmlSchemaComplexType) xsdElement
                    .getSchemaType();
            processComplexType(schema, jaxbNamespace, jaxbNamespacePrefix,
                    xsdComplexType);
        } else if (xsdElement.getSchemaType() instanceof XmlSchemaSimpleType) {
            XmlSchemaSimpleType xsdSimpleType = (XmlSchemaSimpleType) xsdElement
                    .getSchemaType();
            processSimpleType(schema, jaxbNamespace, jaxbNamespacePrefix,
                    xsdElement, xsdSimpleType);
        }

        if (_log.isDebugEnabled()) {
            _log.debug("process ended for element = " + xsdElement.getName());
        }
    }

    /**
     * Process an XML schema simple type.
     * <p/>
     * Only anonymous simple types get annotated. The reason for this is that
     * named simple types do not originate from cob2xsd and are probably more
     * complex than what we anticipate here.
     * <p/>
     * Anonymous simple types might contain enumeration facets. If that's the
     * case then we inject annotations.
     * 
     * @param schema the XML Schema
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param xsdElement the XML Schema element which type we are processing
     * @param xsdSimpleType the XML Schema type to process
     */
    @SuppressWarnings("unchecked")
    protected void processSimpleType(final XmlSchema schema,
            final String jaxbNamespace, final String jaxbNamespacePrefix,
            final XmlSchemaElement xsdElement,
            final XmlSchemaSimpleType xsdSimpleType) {

        if (_log.isDebugEnabled()) {
            _log.debug("process started for simple type = "
                    + xsdSimpleType.getName());
        }
        if (xsdSimpleType.getName() != null) {
            return;
        }

        XmlSchemaSimpleTypeContent content = xsdSimpleType.getContent();
        if (content == null
                || !(content instanceof XmlSchemaSimpleTypeRestriction)) {
            return;
        }
        XmlSchemaSimpleTypeRestriction restriction = (XmlSchemaSimpleTypeRestriction) content;
        if (restriction.getFacets() == null) {
            return;
        }
        XmlSchemaObjectCollection collection = restriction.getFacets();
        boolean hasEnumeration = false;
        for (Iterator < XmlSchemaObject > i = collection.getIterator(); i
                .hasNext();) {
            XmlSchemaObject facet = i.next();
            if (facet instanceof XmlSchemaEnumerationFacet) {
                hasEnumeration = true;
                XmlSchemaEnumerationFacet enumerationFacet = (XmlSchemaEnumerationFacet) facet;
                injectJaxbTypeSafeEnumMemberAnnotation(jaxbNamespace,
                        jaxbNamespacePrefix, enumerationFacet, enumerationFacet
                                .getValue().toString());
            }
        }
        if (hasEnumeration) {
            injectJaxbTypeSafeEnumClassAnnotation(jaxbNamespace,
                    jaxbNamespacePrefix, xsdSimpleType, xsdElement.getName());
        }

        if (_log.isDebugEnabled()) {
            _log.debug("process ended for simple type = "
                    + xsdSimpleType.getName());
        }
    }

    /**
     * Process an XML schema complex type.
     * 
     * @param schema the XML Schema
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param xsdComplexType the XML Schema type to process
     */
    protected void processComplexType(final XmlSchema schema,
            final String jaxbNamespace, final String jaxbNamespacePrefix,
            final XmlSchemaComplexType xsdComplexType) {
        if (_log.isDebugEnabled()) {
            _log.debug("process started for complex type = "
                    + xsdComplexType.getName());
        }

        processParticle(schema, jaxbNamespace, jaxbNamespacePrefix,
                xsdComplexType.getQName(), xsdComplexType.getParticle());

        if (_log.isDebugEnabled()) {
            _log.debug("process ended for complex type = "
                    + xsdComplexType.getName());
        }
    }

    /**
     * A particle is usually all or sequence. It contains a collection of other
     * schema objects that need to be processed.
     * 
     * @param schema the XML Schema
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param parentName the name of the parent schema object for logging
     * @param particle the particle schema object
     */
    protected void processParticle(final XmlSchema schema,
            final String jaxbNamespace, final String jaxbNamespacePrefix,
            final QName parentName, final XmlSchemaParticle particle) {

        if (particle == null) {
            return;
        }

        if (particle instanceof XmlSchemaSequence) {
            XmlSchemaSequence sequence = (XmlSchemaSequence) particle;
            processCollectionElements(schema, jaxbNamespace,
                    jaxbNamespacePrefix, sequence.getItems());

        } else if (particle instanceof XmlSchemaAll) {
            XmlSchemaAll all = (XmlSchemaAll) particle;
            processCollectionElements(schema, jaxbNamespace,
                    jaxbNamespacePrefix, all.getItems());
        }

    }

    /**
     * Create a typesafeEnumClass markup. This marks the simple Type as eligible
     * to become a separate Enum class.
     * 
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param xsdSimpleType the simple type to annotate
     * @param elementName the name of the element whose simple type we are
     *            dealing with.
     */
    protected void injectJaxbTypeSafeEnumClassAnnotation(
            final String jaxbNamespace, final String jaxbNamespacePrefix,
            final XmlSchemaSimpleType xsdSimpleType, final String elementName) {
        XmlSchemaAppInfo appInfo = getXmlSchemaAppInfo(xsdSimpleType);
        DocumentFragment markupParent = getMarkupParent(appInfo);

        Element typesafeEnumClassEl = getElement(markupParent, jaxbNamespace,
                jaxbNamespacePrefix, JAXB_TYPESAFEENUMCLASS);
        typesafeEnumClassEl.setAttribute(JAXB_TYPESAFEENUMCLASS_NAME,
                NameUtil.toClassName(elementName));

        appInfo.setMarkup(markupParent.getChildNodes());
    }

    /**
     * Create a typesafeEnumMember markup. This provides a legal java identifier
     * for an enumeration value. Since these are constant values, we follow the
     * naming convention for static fields (all uppercase).
     * 
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param enumerationFacet the enumeration facet to annotate
     * @param value the enumeration value.
     */
    protected void injectJaxbTypeSafeEnumMemberAnnotation(
            final String jaxbNamespace, final String jaxbNamespacePrefix,
            final XmlSchemaEnumerationFacet enumerationFacet, final String value) {
        XmlSchemaAppInfo appInfo = getXmlSchemaAppInfo(enumerationFacet);
        DocumentFragment markupParent = getMarkupParent(appInfo);

        Element typesafeEnumMemberEl = getElement(markupParent, jaxbNamespace,
                jaxbNamespacePrefix, JAXB_TYPESAFEENUMMEMBER);
        typesafeEnumMemberEl.setAttribute(
                JAXB_TYPESAFEENUMMEMBER_NAME,
                DEFAULT_ENUMERATION_MEMBER_PREFIX
                        + NameUtil.toVariableName(value).toUpperCase());

        appInfo.setMarkup(markupParent.getChildNodes());
    }

    /**
     * @return the bindings parameters
     */
    public JaxbGenModel getJaxbGenModel() {
        return _jaxbGenModel;
    }
}
