package com.legstar.jaxb.gen;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.StringTokenizer;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.constants.Constants;
import org.apache.ws.commons.schema.utils.NamespaceMap;
import org.apache.ws.commons.schema.utils.NamespacePrefixList;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;

import com.legstar.coxb.CobolMarkup;

/**
 * This class implements the methods needed to inject custom JAXB bindings
 * into an XML Schema.
 * <p/>
 * The bindings are necessary so that XJC generates the correct JAXB Classes.
 * <p/>
 * This will not replace any customization that the user might have added
 * manually in the XML Schema.
 * 
 */
public class CobolJAXBCustomizer {

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

    /** This builder is used for annotation markup elements. */
    private final DocumentBuilder _db;

    /** Default XML schema character set. */
    private static final String DEFAULT_XSD_CHARSET = "UTF-8";

    /** The bindings parameters. */
    private CobolJAXBXJBModel _xjbModel;

    /**
     * Construct the customizer.
     * 
     * @param xjbModel the bindinds parameters
     * @throws ParserConfigurationException if can't configure a DOM parser
     */
    public CobolJAXBCustomizer(final CobolJAXBXJBModel xjbModel)
            throws ParserConfigurationException {
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        _db = docFac.newDocumentBuilder();
        _xjbModel = xjbModel;
    }

    /**
     * Adds JAXB custom bindings to an XML Schema.
     * 
     * @param sourceXsdFile the source XML Schema file
     * @param targetXsdFile the target, customized, XML Schema file
     * @throws IOException if output file cannot be written
     */
    public void customize(final File sourceXsdFile,
            final File targetXsdFile)
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
            final String sourceXsdCharset,
            final File targetXsdFile,
            final String targetXsdCharset)
            throws IOException {
        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        XmlSchema xsd = schemaCol.read(
                new InputStreamReader(new FileInputStream(
                                sourceXsdFile), sourceXsdCharset), null);

        String jaxbPrefix = injectJAXBNamespace(xsd);
        injectJAXBExtensionAttributes(xsd, jaxbPrefix);
        _xjbModel.injectAnnotations(xsd, JAXB_NAMESPACE, jaxbPrefix,
                _db.newDocument());

        Writer writer = new OutputStreamWriter(new FileOutputStream(
                targetXsdFile), targetXsdCharset);
        xsd.write(writer);
        writer.close();
    }

    /**
     * Searches the schema element for the JAXB namespace. If the namespace is
     * found, the associated prefix is returned otherwise, the JAXB namespace
     * is added to the schema.
     * 
     * @param xsd the XML Schema
     * @return the JAXB namespace prefix
     */
    public String injectJAXBNamespace(final XmlSchema xsd) {
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
    @SuppressWarnings("unchecked")
    public void injectJAXBExtensionAttributes(final XmlSchema xsd,
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
         * JAXB extension prefixes might already be present in which case
         * we make sure the legstar extension is there too.
         * Extension prefixes are specified as a whitespace-separated list of
         * namespace prefixes.
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
     * @return the bindings parameters
     */
    public CobolJAXBXJBModel getXjbModel() {
        return _xjbModel;
    }
}
