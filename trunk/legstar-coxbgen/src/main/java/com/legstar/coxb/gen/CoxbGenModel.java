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
package com.legstar.coxb.gen;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.tools.ant.BuildException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractAntBuildModel;
import com.legstar.jaxb.gen.CobolJAXBXJBModel;
import com.sun.xml.bind.api.impl.NameConverter;

/**
 * A model usable for Binding classes generation.
 * Groups all the data needed to generate a set of binding classes and
 * an intermediary set of jaxb classes from a COBOL-annotated XML schema.
 * Gathers all parameters that are needed during the lifetime of the generation
 * process. This allows more parameters to be added without too much impact on
 * other classes.
 * 
 */
public class CoxbGenModel extends AbstractAntBuildModel {

    /** The JAXB/COXB annotated XML schema file. */
    private File _xsdFile;

    /** The package name used for JAXB classes. */
    private String _jaxbPackageName;

    /** JAXB binding customization made available. */
    private CobolJAXBXJBModel _jaxbXjbModel = new CobolJAXBXJBModel();

    /**
     * The location where JAXB classes sources live.
     * This is not strictly needed for binding generation but is useful
     * when this model is also used for JAXB classes generation.
     */
    private File _jaxbSrcDir;

    /** The location where JAXB compiled classes live. */
    private File _jaxbBinDir;

    /** A set of Jaxb root class names to generated binding classes for. */
    private List < String > _jaxbRootClassNames;

    /** The target package name for generated binding classes. */
    private String _coxbPackageName;

    /** The target directory where source files will be created. */
    private File _coxbSrcDir;

    /** The target directory where binary files will be created. */
    private File _coxbBinDir;

    /**
     * An optional runtime alternative to the Jaxb package name used at
     * generation time.
     */
    private String _alternativePackageName;

    /**
     * At runtime, if a alternativePackageName is specified, this alternative
     * factory can be used rather than the JAXB one.
     */
    private String _alternativeFactoryName;

    /** Generate Host to XML transformers. */
    private boolean _xmlTransformers;

    /** The additional package level for generated binding classes. */
    private static final String COXB_PACKAGE_SUFFIX = "bind";

    /** This generator name. */
    public static final String COXB_GENERATOR_NAME =
            "LegStar Binding generator";

    /** This velocity template. */
    public static final String COXB_VELOCITY_MACRO_NAME =
            "vlc/build-coxb-xml.vm";

    /** The XML Schema namespace needed to retrieve the target namespace. */
    private static final String XSD_NS = "http://www.w3.org/2001/XMLSchema";

    /** The XML SChema element name. */
    private static final String XSD_ELEMENT_NAME = "schema";

    /** The XML Schema targetnamespace attribute. */
    private static final String XSD_TARGETNAMESPACE_ATTR = "targetNamespace";

    /** A general purpose DOM document builder. */
    private DocumentBuilder _docBuilder;

    /** Borrowed from XJC. Serves for XML to Java name conversions. */
    private NameConverter _xjNameConverter = new NameConverter.Standard();

    /**
     * Creates an ant build script file ready for binding generation.
     * 
     * @param scriptFile
     *            the script file that must be created
     * @throws CodeGenMakeException
     *             if generation fails
     */
    public void generateBuild(final File scriptFile)
            throws CodeGenMakeException {
        super.generateBuild(COXB_GENERATOR_NAME, COXB_VELOCITY_MACRO_NAME,
                scriptFile, "UTF-8");
    }

    /**
     * Adds a jaxb root class name to generate a binding class for.
     * 
     * @param className The JAXB root class name to set.
     */
    public void addJaxbRootClassName(
            final String className) {
        if (_jaxbRootClassNames == null) {
            _jaxbRootClassNames = new ArrayList < String >();
        }
        if (!_jaxbRootClassNames.contains(className)) {
            _jaxbRootClassNames.add(className);
        }
    }

    /**
     * @return the XML schema file
     */
    public File getXsdFile() {
        return _xsdFile;
    }

    /**
     * @param xsdFile the XML schema file to set
     */
    public void setXsdFile(
            final File xsdFile) {
        _xsdFile = xsdFile;
    }

    /**
     * @return the current target directory
     */
    public File getCoxbSrcDir() {
        return _coxbSrcDir;
    }

    /**
     * @param targetDir the target directory to set
     */
    public void setCoxbSrcDir(final File targetDir) {
        _coxbSrcDir = targetDir;
    }

    /**
     * @return the location where JAXB classes live
     */
    public File getJaxbBinDir() {
        return _jaxbBinDir;
    }

    /**
     * @param jaxbBinDir the JAXB location to set
     */
    public void setJaxbBinDir(final File jaxbBinDir) {
        _jaxbBinDir = jaxbBinDir;
    }

    /**
     * @return the package name used for JAXB classes
     * @throws CoxbGenException if pacakage could not le extracted from XML
     *             schema
     */
    public String getJaxbPackageName() throws CoxbGenException {
        if (_jaxbPackageName == null) {
            _jaxbPackageName = getJaxbPackageNameFromXsd(getXsdFile());
        }
        return _jaxbPackageName;
    }

    /**
     * @param jaxbPackageName the JAXB classes package name to set
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        _jaxbPackageName = jaxbPackageName;
    }

    /**
     * @return if IsSet Methods should be generated
     */
    public boolean isGenerateIsSetMethod() {
        return _jaxbXjbModel.isGenerateIsSetMethod();
    }

    /**
     * @param generateIsSetMethod if IsSet Methods should be generated
     */
    public void setGenerateIsSetMethod(final boolean generateIsSetMethod) {
        _jaxbXjbModel.setGenerateIsSetMethod(generateIsSetMethod);
    }

    /**
     * @return the serialization unique ID. (All JAXB classes must be
     *         serializable for LegStar)
     */
    public long getSerializableUid() {
        return _jaxbXjbModel.getSerializableUid();
    }

    /**
     * @param serializableUid the serialization unique ID. (All JAXB classes
     *            must be serializable for LegStar)
     */
    public void setSerializableUid(final long serializableUid) {
        _jaxbXjbModel.setSerializableUid(serializableUid);
    }

    /**
     * @return the prefix to add to type names
     */
    public String getTypeNamePrefix() {
        return _jaxbXjbModel.getTypeNamePrefix();
    }

    /**
     * @param typeNamePrefix the prefix to add to type names
     */
    public void setTypeNamePrefix(final String typeNamePrefix) {
        _jaxbXjbModel.setTypeNamePrefix(typeNamePrefix);
    }

    /**
     * @return the suffix to add to type names
     */
    public String getTypeNameSuffix() {
        return _jaxbXjbModel.getTypeNameSuffix();
    }

    /**
     * @param typeNameSuffix the suffix to add to type names
     */
    public void setTypeNameSuffix(final String typeNameSuffix) {
        _jaxbXjbModel.setTypeNameSuffix(typeNameSuffix);
    }

    /**
     * @return the prefix to add to element names
     */
    public String getElementNamePrefix() {
        return _jaxbXjbModel.getElementNamePrefix();
    }

    /**
     * @param elementNamePrefix the prefix to add to element names
     */
    public void setElementNamePrefix(final String elementNamePrefix) {
        _jaxbXjbModel.setElementNamePrefix(elementNamePrefix);
    }

    /**
     * @return the suffix to add to element names
     */
    public String getElementNameSuffix() {
        return _jaxbXjbModel.getElementNameSuffix();
    }

    /**
     * @param elementNameSuffix the suffix to add to element names
     */
    public void setElementNameSuffix(final String elementNameSuffix) {
        _jaxbXjbModel.setElementNameSuffix(elementNameSuffix);
    }

    /**
     * @return the package name for generated binding classes
     * @throws CoxbGenException if package name cannot be extracted from XML
     *             Schema
     */
    public String getCoxbPackageName() throws CoxbGenException {
        if (_coxbPackageName == null) {
            return getJaxbPackageName() + '.' + COXB_PACKAGE_SUFFIX;
        }
        return _coxbPackageName;
    }

    /**
     * @param coxbPackageName package name for generated binding classes to set
     */
    public void setCoxbPackageName(final String coxbPackageName) {
        _coxbPackageName = coxbPackageName;
    }

    /**
     * @return the optional runtime alternative to the Jaxb package name used at
     *         generation time
     */
    public String getAlternativePackageName() {
        return _alternativePackageName;
    }

    /**
     * @param alternativePackageName the optional runtime alternative to the
     *            Jaxb package name used at generation time
     */
    public void setAlternativePackageName(
            final String alternativePackageName) {
        _alternativePackageName = alternativePackageName;
    }

    /**
     * @return the alternate factory to used rather than the JAXB one.
     */
    public String getAlternativeFactoryName() {
        return _alternativeFactoryName;
    }

    /**
     * @param targetFactoryName the alternate factory to used rather than the
     *            JAXB one
     */
    public void setAlternativeFactoryName(
            final String targetFactoryName) {
        _alternativeFactoryName = targetFactoryName;
    }

    /**
     * This is not strictly needed for binding generation but is useful
     * when this model is also used for JAXB classes generation.
     * 
     * @return the location where JAXB classes sources live
     */
    public File getJaxbSrcDir() {
        return _jaxbSrcDir;
    }

    /**
     * @param jaxbSrcDir the location where JAXB classes sources live to set
     */
    public void setJaxbSrcDir(final File jaxbSrcDir) {
        _jaxbSrcDir = jaxbSrcDir;
    }

    /**
     * @return the set of Jaxb root class names to generated binding classes for
     */
    public List < String > getJaxbRootClassNames() {
        return _jaxbRootClassNames;
    }

    /**
     * @param jaxbRootClassNames the set of Jaxb root class names to generated
     *            binding classes for to set
     */
    public void setJaxbRootClassNames(
            final List < String > jaxbRootClassNames) {
        _jaxbRootClassNames = jaxbRootClassNames;
    }

    /**
     * @return the target directory where binary files will be created
     */
    public File getCoxbBinDir() {
        return _coxbBinDir;
    }

    /**
     * @param coxbBinDir the target directory where binary files will be created
     *            to set
     */
    public void setCoxbBinDir(final File coxbBinDir) {
        _coxbBinDir = coxbBinDir;
    }

    /**
     * @return the JAXB binding customization
     */
    public CobolJAXBXJBModel getJaxbXjbModel() {
        return _jaxbXjbModel;
    }

    /**
     * @param jaxbXjbModel the JAXB binding customization to set
     */
    public void setJaxbXjbModel(final CobolJAXBXJBModel jaxbXjbModel) {
        _jaxbXjbModel = jaxbXjbModel;
    }

    /**
     * @return true if Host to XML transformers generation is turned on
     */
    public boolean isXmlTransformers() {
        return _xmlTransformers;
    }

    /**
     * @param xmlTransformers true if Host to XML transformers generation is
     *            turned on
     */
    public void setXmlTransformers(final boolean xmlTransformers) {
        _xmlTransformers = xmlTransformers;
    }

    /**
     * Extracts the JAXB package name from the XML schema targetNamespace.
     * <p/>
     * We delegate code to XJC which already knows how to turn a targetnamespace
     * into a package name.
     * 
     * @param xsdFile the XML schema file
     * @return the package name
     * @throws CoxbGenException if package name cannot be retrieved from XSD
     */
    public String getJaxbPackageNameFromXsd(final File xsdFile)
            throws CoxbGenException {
        if (xsdFile == null) {
            return null;
        }
        try {
            Document doc = getDocBuilder().parse(xsdFile);
            NodeList listOfElements = doc.getElementsByTagNameNS(XSD_NS,
                    XSD_ELEMENT_NAME);
            if (listOfElements == null || listOfElements.getLength() == 0) {
                throw (new BuildException(
                        "No target namespace in XML schema file"));
            }
            String targetNamespace = ((Element) listOfElements.item(0))
                    .getAttribute(
                    XSD_TARGETNAMESPACE_ATTR);
            if (targetNamespace == null || targetNamespace.length() == 0) {
                throw (new BuildException(
                        "No target namespace in XML schema file"));
            }
            return _xjNameConverter.toPackageName(targetNamespace);

        } catch (SAXException e) {
            throw (new CoxbGenException(
                    "SAXException " + e.getMessage()));
        } catch (IOException e) {
            throw (new CoxbGenException(
                    "IOException " + e.getMessage()));
        }
    }

    /**
     * Creates a DOM document builder if none has already been created.
     * 
     * @return an instance of a DOM builder
     * @throws CoxbGenException if DOM builder cannot be created
     */
    protected DocumentBuilder getDocBuilder() throws CoxbGenException {
        if (_docBuilder == null) {

            try {
                DocumentBuilderFactory docBuilderFactory =
                        DocumentBuilderFactory.newInstance();
                docBuilderFactory.setNamespaceAware(true);
                _docBuilder = docBuilderFactory.newDocumentBuilder();
            } catch (ParserConfigurationException e) {
                throw new CoxbGenException(e);
            }
        }
        return _docBuilder;
    }

    /**
     * @return a complete trace of parameters values
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        sb.append("COXB source dir: " + getCoxbSrcDir());
        if (getCoxbBinDir() != null) {
            sb.append(", ");
            sb.append("COXB binaries dir: " + getCoxbBinDir());
        }
        if (getXsdFile() != null) {
            sb.append(", ");
            sb.append("XML schema: " + getXsdFile());
        }
        sb.append(", ");
        try {
            sb.append("COXB package: " + getCoxbPackageName());
        } catch (CoxbGenException e1) {
            sb.append("COXB package: " + e1.getMessage());
        }
        if (isXmlTransformers()) {
            sb.append(", ");
            sb.append("XML transformers: " + isXmlTransformers());
        }
        if (getJaxbSrcDir() != null) {
            sb.append(", ");
            sb.append("JAXB source dir: " + getJaxbSrcDir());
        }
        if (getJaxbBinDir() != null) {
            sb.append(", ");
            sb.append("JAXB binaries dir: " + getJaxbBinDir());
        }
        sb.append(", ");
        try {
            sb.append("JAXB package: " + getJaxbPackageName());
        } catch (CoxbGenException e) {
            sb.append("JAXB package: " + e.getMessage());
        }
        sb.append(", ");
        sb.append("JAXB binding customization: "
                + " " + getJaxbXjbModel().toString());
        if (getJaxbRootClassNames() != null) {
            sb.append(", ");
            sb.append("{");
            boolean next = false;
            for (String jaxbRootClassName : getJaxbRootClassNames()) {
                if (next) {
                    sb.append(", ");
                } else {
                    next = true;
                }
                sb.append("JAXB root class name: "
                        + " " + jaxbRootClassName);
            }
            sb.append("}");
        }
        if (getAlternativePackageName() != null) {
            sb.append(", ");
            sb.append("Alternate JAXB package: " + getAlternativePackageName());
        }
        if (getAlternativeFactoryName() != null) {
            sb.append(", ");
            sb.append("Alternate JAXB factory: " + getAlternativeFactoryName());
        }
        sb.append("}");
        return sb.toString();
    }

}
