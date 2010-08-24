package com.legstar.jaxb.gen;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.codegen.CodeGenVelocityException;

/**
 * Parameters used for the JAXB external binding customization file (XJB)
 * generation.
 * 
 */
public class CobolJAXBXJBModel {

    /** A template for a JAXB external binding customization file. */
    public static final String XJB_TEMPLATE = "vlc/bindings.xjb.vm";

    /** Character set used for the XJB file content. */
    public static final String XJB_CHARSET = "UTF-8";

    /** Velocity identifier for this generator. */
    public static final String XJB_GENERATOR_NAME = "XJB Generator";

    /** Generates isSet methods to check for nulls. */
    private boolean _generateIsSetMethod = true;

    /** JAXB annotation parent element name. */
    private static final String JAXB_DUMMY_PARENT = "jaxbElements";

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

    /**
     * The serialization unique ID. (All JAXB classes must be serializable for
     * LegStar).
     */
    private long _serializableUid = 1L;

    /** The physical location of the XML Schema. */
    private String _xsdLocation;

    /** Prefix to add to type names. */
    private String _typeNamePrefix;

    /** Suffix to add to type names. */
    private String _typeNameSuffix;

    /** Prefix to add to element names. */
    private String _elementNamePrefix;

    /** Suffix to add to element names. */
    private String _elementNameSuffix;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

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
     * Given an XML Schema, this will inject or replace custom
     * JAXB bindings annotations.
     * 
     * @param xsd the XML Schema
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param doc a DOM document to hold nodes used for annotations
     */
    public void injectAnnotations(final XmlSchema xsd,
            final String jaxbNamespace,
            final String jaxbNamespacePrefix,
            final Document doc) {
        xsd.setAnnotation(createSchemaAnnotations(jaxbNamespace,
                jaxbNamespacePrefix, doc));
    }

    /**
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
     * @param jaxbNamespace the JAXB namespace
     * @param jaxbNamespacePrefix the JAXB namespace prefix
     * @param doc a DOM document to hold nodes used for annotations
     * @return a schema level annotation
     * */
    protected XmlSchemaAnnotation createSchemaAnnotations(
            final String jaxbNamespace,
            final String jaxbNamespacePrefix,
            final Document doc) {

        Element dummyEl = doc.createElementNS(jaxbNamespace,
                jaxbNamespacePrefix
                        + ':' + JAXB_DUMMY_PARENT);

        // Create the global bindings annotation
        Element globalbindingsEl = doc.createElementNS(jaxbNamespace,
                jaxbNamespacePrefix
                        + ':' + JAXB_GLOBALBINDINGS);
        globalbindingsEl.setAttribute(JAXB_GENERATEISSETMETHOD, Boolean
                .toString(isGenerateIsSetMethod()));
        Element serializableEl = doc.createElementNS(jaxbNamespace,
                jaxbNamespacePrefix
                        + ':' + JAXB_SERIALIZABLE);
        serializableEl.setAttribute(JAXB_UID, Long
                .toString(getSerializableUid()));
        globalbindingsEl.appendChild(serializableEl);

        dummyEl.appendChild(globalbindingsEl);

        // Create the schema bindings annotation
        Element schemabindingsEl = doc.createElementNS(jaxbNamespace,
                jaxbNamespacePrefix
                        + ':' + JAXB_SCHEMABINDINGS);

        if (needXmlTransform()) {
            Element xmltransformEl = doc.createElementNS(jaxbNamespace,
                    jaxbNamespacePrefix
                            + ':' + JAXB_XMLTRANSFORM);

            if (needTypeNameXmlTransform()) {
                Element typenameEl = doc
                        .createElementNS(jaxbNamespace, jaxbNamespacePrefix
                                + ':' + JAXB_TYPENAME);
                if (getTypeNamePrefix() != null) {
                    typenameEl.setAttribute(JAXB_PREFIX,
                            getTypeNamePrefix());
                }
                if (getTypeNameSuffix() != null) {
                    typenameEl.setAttribute(JAXB_SUFFIX,
                            getTypeNameSuffix());
                }
                xmltransformEl.appendChild(typenameEl);
            }

            if (needElementNameXmlTransform()) {
                Element elementnameEl = doc
                        .createElementNS(jaxbNamespace, jaxbNamespacePrefix
                                + ':' + JAXB_ELEMENTNAME);
                if (getElementNamePrefix() != null) {
                    elementnameEl.setAttribute(JAXB_PREFIX,
                            getElementNamePrefix());
                }
                if (getElementNameSuffix() != null) {
                    elementnameEl.setAttribute(JAXB_SUFFIX,
                            getElementNameSuffix());
                }
                xmltransformEl.appendChild(elementnameEl);
            }

            schemabindingsEl.appendChild(xmltransformEl);
        }

        dummyEl.appendChild(schemabindingsEl);

        XmlSchemaAnnotation annotation = new XmlSchemaAnnotation();
        XmlSchemaAppInfo appInfo = new XmlSchemaAppInfo();
        NodeList markup = dummyEl.getChildNodes();
        appInfo.setMarkup(markup);
        annotation.getItems().add(appInfo);
        return annotation;
    }

    /**
     * @return true if an XmlTransform annotation is needed
     */
    public boolean needXmlTransform() {
        return needTypeNameXmlTransform()
                || needElementNameXmlTransform();
    }

    /**
     * @return true if an XmlTransform/typeName annotation is needed
     */
    public boolean needTypeNameXmlTransform() {
        return getTypeNamePrefix() != null
                || getTypeNameSuffix() != null;
    }

    /**
     * @return true if an XmlTransform/elementName annotation is needed
     */
    public boolean needElementNameXmlTransform() {
        return getElementNamePrefix() != null
                || getElementNameSuffix() != null;
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

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        sb.append("isGenerateIsSetMethod: " + isGenerateIsSetMethod());
        sb.append(", ");
        sb.append("serializableUid: " + getSerializableUid());
        sb.append(", ");
        sb.append("xsdLocation: " + getXsdLocation());
        sb.append(", ");
        sb.append("elementNamePrefix: " + getElementNamePrefix());
        sb.append(", ");
        sb.append("elementNameSuffix: " + getElementNameSuffix());
        sb.append(", ");
        sb.append("typeNamePrefix: " + getTypeNamePrefix());
        sb.append(", ");
        sb.append("typeNameSuffix: " + getTypeNameSuffix());
        sb.append("}");
        return sb.toString();

    }

}
