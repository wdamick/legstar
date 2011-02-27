/*******************************************************************************
 * Copyright (c) 2010 LegSem.
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
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.XmlSchemaObject;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.codegen.CodeGenVelocityException;
import com.legstar.codegen.models.AbstractPropertiesModel;

/**
 * Parameters used for the COBOL-annotated JAXB classes generation.
 * 
 */
public class JaxbGenModel extends AbstractPropertiesModel {

    /** A template for a JAXB external binding customization file. */
    public static final String XJB_TEMPLATE = "vlc/bindings.xjb.vm";

    /** Character set used for the XJB file content. */
    public static final String XJB_CHARSET = "UTF-8";

    /** Velocity identifier for this generator. */
    public static final String XJB_GENERATOR_NAME = "XJB Generator";

    /* ====================================================================== */
    /* Following are default field values. = */
    /* ====================================================================== */

    /** Default value for generate isSet method. */
    public static final boolean DEFAULT_GENERATEISSETMETHOD = true;

    /** Default value for serializable ID. */
    public static final long DEFAULT_SERIALIZABLE_ID = 1L;

    /** Default value for use of internal bindings. */
    public static final boolean DEFAULT_INTERNALBINDINGS = true;

    /** Default value for use ECI compatible mode. */
    public static final boolean DEFAULT_ECICOMPATIBLE = false;

    /* ====================================================================== */
    /* Following are XML identifiers for XJB Binding. = */
    /* ====================================================================== */

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

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** Physical location of the XML schema. */
    public static final String JAXB_XSD_LOCATION = "xsdLocation";

    /** Generate isSet methods. */
    public static final String JAXB_XJB_ISGENERATEISSETMETHOD = "generateIsSetMethod";

    /** Serializable ID. */
    public static final String JAXB_XJB_SERIALIZABLE_ID = "serializableID";

    /** Element name prefix. */
    public static final String JAXB_XJB_ELEMENTNAME_PREFIX = "elementNamePrefix";

    /** Element name suffix. */
    public static final String JAXB_XJB_ELEMENTNAME_SUFFIX = "elementNameSuffix";

    /** Type name prefix. */
    public static final String JAXB_XJB_TYPENAME_PREFIX = "typeNamePrefix";

    /** Type name suffix. */
    public static final String JAXB_XJB_TYPENAME_SUFFIX = "typeNameSuffix";

    /** JAXB package name. */
    public static final String JAXB_PACKAGENAME = "jaxbPackageName";

    /** JAXB uses internal bindings. */
    public static final String JAXB_INTERNALBINDINGS = "internalBindings";

    /** Use the ECI naming conventions. */
    public static final String JAXB_ECICOMPATIBLE = "eciCompatible";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** The physical location of the XML Schema. */
    private String _xsdLocation;

    /**
     * The serialization unique ID. (All JAXB classes must be serializable for
     * LegStar).
     */
    private long _serializableUid = 1L;

    /** Generates isSet methods to check for nulls. */
    private boolean _generateIsSetMethod = DEFAULT_GENERATEISSETMETHOD;

    /** Prefix to add to type names. */
    private String _typeNamePrefix;

    /** Suffix to add to type names. */
    private String _typeNameSuffix;

    /** Prefix to add to element names. */
    private String _elementNamePrefix;

    /** Suffix to add to element names. */
    private String _elementNameSuffix;

    /** Whether internal bindings or and external binding should be used. */
    private boolean _internalBindings = DEFAULT_INTERNALBINDINGS;

    /** Whether the ECI naming conventions should be used. */
    private boolean _eciCompatible = DEFAULT_ECICOMPATIBLE;

    /**
     * From XJC. If specified, generated code will be placed under this Java
     * package. This option is equivalent to the "-p" command-line switch.
     * Optional parameter.
     */
    private String _jaxbPackageName;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * A no-Arg constructor.
     */
    public JaxbGenModel() {

    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public JaxbGenModel(final Properties props) {
        setXsdLocation(getString(props, JAXB_XSD_LOCATION, null));
        setGenerateIsSetMethod(getBoolean(props,
                JAXB_XJB_ISGENERATEISSETMETHOD, DEFAULT_GENERATEISSETMETHOD));
        setSerializableUid(getLong(props, JAXB_XJB_SERIALIZABLE_ID,
                DEFAULT_SERIALIZABLE_ID));
        setElementNamePrefix(getString(props, JAXB_XJB_ELEMENTNAME_PREFIX, null));
        setElementNameSuffix(getString(props, JAXB_XJB_ELEMENTNAME_SUFFIX, null));
        setTypeNamePrefix(getString(props, JAXB_XJB_TYPENAME_PREFIX, null));
        setTypeNameSuffix(getString(props, JAXB_XJB_TYPENAME_SUFFIX, null));
        setJaxbPackageName(getString(props, JAXB_PACKAGENAME, null));
        setInternalBindings(getBoolean(props, JAXB_INTERNALBINDINGS,
                DEFAULT_INTERNALBINDINGS));
        setEciCompatible(getBoolean(props, JAXB_ECICOMPATIBLE,
                DEFAULT_ECICOMPATIBLE));
    }

    /**
     * Creates an external binding customization file ready for JAXB.
     * 
     * @param targetFile the xjb file that must be created
     * @throws CodeGenMakeException if generation fails
     */
    public void generateXjb(final File targetFile) throws CodeGenMakeException {

        try {
            CodeGenUtil.initVelocity();
            CodeGenHelper helper = new CodeGenHelper();
            Map < String, Object > parameters = new HashMap < String, Object >();
            parameters.put("helper", helper);

            CodeGenUtil.processTemplate(XJB_GENERATOR_NAME, XJB_TEMPLATE,
                    "xjbModel", this, parameters, targetFile, XJB_CHARSET);

            if (_log.isDebugEnabled()) {
                _log.debug("External Bindings file:" + targetFile);
                for (Object line : FileUtils.readLines(targetFile)) {
                    _log.debug(line);
                }
            }

        } catch (CodeGenVelocityException e) {
            throw new CodeGenMakeException(e);
        } catch (IOException e) {
            throw new CodeGenMakeException(e);
        }
    }

    /**
     * Given an XML Schema, this will inject or replace custom JAXB bindings
     * annotations.
     * <p/>
     * The generated schema holds JAXB annotations needed when, later on, the
     * schema is used to generate JAXB classes. The markup looks like this:
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
     * @param doc a DOM document to hold nodes used for annotations
     */
    @SuppressWarnings("unchecked")
    public void injectJaxbAnnotations(final XmlSchema schema,
            final String jaxbNamespace, final String jaxbNamespacePrefix,
            final Document doc) {

        /* Schema object might already be annotated */
        XmlSchemaAnnotation annotation = null;
        if (schema.getAnnotation() == null) {
            annotation = new XmlSchemaAnnotation();
        } else {
            annotation = schema.getAnnotation();
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

        /* Preserve any previous markup from appinfo */
        NodeList markup = appInfo.getMarkup();
        DocumentFragment markupParent = null;
        if (markup == null || markup.getLength() == 0) {
            markupParent = doc.createDocumentFragment();
        } else {
            markupParent = (DocumentFragment) markup.item(0).getParentNode();
        }

        injectJaxbGlobalBindingsAnnotations(markupParent, jaxbNamespace,
                jaxbNamespacePrefix);

        injectJaxbSchemaBindingsAnnotations(markupParent, jaxbNamespace,
                jaxbNamespacePrefix);

        appInfo.setMarkup(markupParent.getChildNodes());
        schema.setAnnotation(annotation);
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

        Element globalbindingsEl = null;
        NodeList nl = markupParent.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            if (nl.item(i) instanceof Element
                    && nl.item(i).getNamespaceURI().equals(jaxbNamespace)
                    && nl.item(i).getLocalName().equals(JAXB_GLOBALBINDINGS)) {
                globalbindingsEl = (Element) nl.item(i);
                break;
            }
        }
        if (globalbindingsEl == null) {
            globalbindingsEl = markupParent.getOwnerDocument().createElementNS(
                    jaxbNamespace,
                    jaxbNamespacePrefix + ':' + JAXB_GLOBALBINDINGS);
            markupParent.appendChild(globalbindingsEl);
        }
        globalbindingsEl.setAttribute(JAXB_GENERATEISSETMETHOD,
                Boolean.toString(isGenerateIsSetMethod()));

        /* ECI expects indexed collections not Lists (the default JAXB) */
        if (isEciCompatible()) {
            globalbindingsEl.setAttribute(JAXB_COLLECTION_TYPE, "indexed");
        }

        injectJaxbSerializableAnnotation(globalbindingsEl, jaxbNamespace,
                jaxbNamespacePrefix);

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
                Long.toString(getSerializableUid()));
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

        if (needXmlTransform()) {
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
        if (needTypeNameXmlTransform()) {
            injectJaxbTypeNameXmlTransformAnnotation(xmltransformEl,
                    jaxbNamespace, jaxbNamespacePrefix);
        }
        if (needElementNameXmlTransform()) {
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
        if (getTypeNamePrefix() != null) {
            typenamexmltransformEl.setAttribute(JAXB_PREFIX,
                    getTypeNamePrefix());
        }
        if (getTypeNameSuffix() != null) {
            typenamexmltransformEl.setAttribute(JAXB_SUFFIX,
                    getTypeNameSuffix());
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
        if (getElementNamePrefix() != null) {
            elementnamexmltransformEl.setAttribute(JAXB_PREFIX,
                    getElementNamePrefix());
        }
        if (getElementNameSuffix() != null) {
            elementnamexmltransformEl.setAttribute(JAXB_SUFFIX,
                    getElementNameSuffix());
        }
    }

    /**
     * @return true if an XmlTransform annotation is needed
     */
    public boolean needXmlTransform() {
        return needTypeNameXmlTransform() || needElementNameXmlTransform();
    }

    /**
     * @return true if an XmlTransform/typeName annotation is needed
     */
    public boolean needTypeNameXmlTransform() {
        return getTypeNamePrefix() != null || getTypeNameSuffix() != null;
    }

    /**
     * @return true if an XmlTransform/elementName annotation is needed
     */
    public boolean needElementNameXmlTransform() {
        return getElementNamePrefix() != null || getElementNameSuffix() != null;
    }

    /**
     * @return if IsSet Methods should be generated
     */
    public boolean isGenerateIsSetMethod() {
        return _generateIsSetMethod;
    }

    /**
     * @param generateIsSetMethod if IsSet Methods should be generated
     */
    public void setGenerateIsSetMethod(final boolean generateIsSetMethod) {
        _generateIsSetMethod = generateIsSetMethod;
    }

    /**
     * @return the serialization unique ID. (All JAXB classes must be
     *         serializable for LegStar)
     */
    public long getSerializableUid() {
        return _serializableUid;
    }

    /**
     * @param serializableUid the serialization unique ID. (All JAXB classes
     *            must be serializable for LegStar)
     */
    public void setSerializableUid(final long serializableUid) {
        _serializableUid = serializableUid;
    }

    /**
     * @return the physical location of the XML Schema
     */
    public String getXsdLocation() {
        return _xsdLocation;
    }

    /**
     * @param xsdLocation the physical location of the XML Schema
     */
    public void setXsdLocation(final String xsdLocation) {
        _xsdLocation = xsdLocation;
    }

    /**
     * @return the prefix to add to type names
     */
    public String getTypeNamePrefix() {
        return _typeNamePrefix;
    }

    /**
     * @param typeNamePrefix the prefix to add to type names
     */
    public void setTypeNamePrefix(final String typeNamePrefix) {
        _typeNamePrefix = typeNamePrefix;
    }

    /**
     * @return the suffix to add to type names
     */
    public String getTypeNameSuffix() {
        return _typeNameSuffix;
    }

    /**
     * @param typeNameSuffix the suffix to add to type names
     */
    public void setTypeNameSuffix(final String typeNameSuffix) {
        _typeNameSuffix = typeNameSuffix;
    }

    /**
     * @return the prefix to add to element names
     */
    public String getElementNamePrefix() {
        return _elementNamePrefix;
    }

    /**
     * @param elementNamePrefix the prefix to add to element names
     */
    public void setElementNamePrefix(final String elementNamePrefix) {
        _elementNamePrefix = elementNamePrefix;
    }

    /**
     * @return the suffix to add to element names
     */
    public String getElementNameSuffix() {
        return _elementNameSuffix;
    }

    /**
     * @param elementNameSuffix the suffix to add to element names
     */
    public void setElementNameSuffix(final String elementNameSuffix) {
        _elementNameSuffix = elementNameSuffix;
    }

    /**
     * If specified, generated code will be placed under this Java package.
     * 
     * @return Java package name
     */
    public String getJaxbPackageName() {
        return _jaxbPackageName;
    }

    /**
     * If specified, generated code will be placed under this Java package.
     * 
     * @param jaxbPackageName Java package name
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        _jaxbPackageName = jaxbPackageName;
    }

    /**
     * whether internal bindings or and external binding should be used.
     * 
     * @return whether internal bindings or and external binding should be used
     */
    public boolean isInternalBindings() {
        return _internalBindings;
    }

    /**
     * Whether internal bindings or and external binding should be used.
     * 
     * @param internalBindings whether internal bindings or and external binding
     *            should be used
     */
    public void setInternalBindings(final boolean internalBindings) {
        _internalBindings = internalBindings;
    }

    /**
     * Whether the ECI naming conventions should be used.
     * 
     * @return Whether the ECI naming conventions should be used
     */
    public boolean isEciCompatible() {
        return _eciCompatible;
    }

    /**
     * Whether the ECI naming conventions should be used.
     * 
     * @param eciCompatible whether the ECI naming conventions should be used
     */
    public void setEciCompatible(final boolean eciCompatible) {
        _eciCompatible = eciCompatible;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        if (getXsdLocation() != null) {
            putString(props, JAXB_XSD_LOCATION, getXsdLocation());
        }
        putBoolean(props, JAXB_XJB_ISGENERATEISSETMETHOD,
                isGenerateIsSetMethod());
        putLong(props, JAXB_XJB_SERIALIZABLE_ID, getSerializableUid());
        if (getElementNamePrefix() != null) {
            putString(props, JAXB_XJB_ELEMENTNAME_PREFIX,
                    getElementNamePrefix());
        }
        if (getElementNameSuffix() != null) {
            putString(props, JAXB_XJB_ELEMENTNAME_SUFFIX,
                    getElementNameSuffix());
        }
        if (getTypeNamePrefix() != null) {
            putString(props, JAXB_XJB_TYPENAME_PREFIX, getTypeNamePrefix());
        }
        if (getTypeNameSuffix() != null) {
            putString(props, JAXB_XJB_TYPENAME_SUFFIX, getTypeNameSuffix());
        }
        if (getJaxbPackageName() != null) {
            putString(props, JAXB_PACKAGENAME, getJaxbPackageName());
        }
        putBoolean(props, JAXB_INTERNALBINDINGS, isInternalBindings());
        putBoolean(props, JAXB_ECICOMPATIBLE, isEciCompatible());
        return props;
    }

}
