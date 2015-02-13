/*******************************************************************************
 * Copyright (c) 2015 LegSem.
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
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractAntBuildModel;
import com.legstar.coxb.util.NameUtil;
import com.legstar.jaxb.gen.JaxbGenModel;

/**
 * A model usable for Binding classes generation. Groups all the data needed to
 * generate a set of binding classes and an intermediary set of jaxb classes
 * from a COBOL-annotated XML schema. Gathers all parameters that are needed
 * during the lifetime of the generation process. This allows more parameters to
 * be added without too much impact on other classes.
 * 
 */
public class CoxbGenModel extends AbstractAntBuildModel {

    /** This generator name. */
    public static final String COXB_GENERATOR_NAME = "LegStar Binding generator";

    /** This velocity template. */
    public static final String COXB_VELOCITY_MACRO_NAME = "vlc/build-coxb-xml.vm";

    /** The additional package level for generated binding classes. */
    private static final String COXB_PACKAGE_SUFFIX = "bind";

    /* ====================================================================== */
    /* Following are default field values. = */
    /* ====================================================================== */

    /** Default value for XML transformers generation. */
    public static final boolean DEFAULT_ISXMLTRANSFORMERS = true;

    /** Default value for JSON transformers generation. */
    public static final boolean DEFAULT_ISJSONTRANSFORMERS = false;

    /** Default value for transformers compilation. */
    public static final boolean DEFAULT_IS_COMPILE_TRANSFORMERS = false;

    /**
     * When XML Schema does not contain a target namespace, JAXB uses this
     * default package name.
     */
    public static final String DEFAULT_JAXB_PACKAGENAME = "generated";

    /* ====================================================================== */
    /* Following are XML identifiers for XML Schema. = */
    /* ====================================================================== */

    /** The XML Schema namespace needed to retrieve the target namespace. */
    private static final String XSD_NS = "http://www.w3.org/2001/XMLSchema";

    /** The XML SChema element name. */
    private static final String XSD_ELEMENT_NAME = "schema";

    /** The XML Schema targetnamespace attribute. */
    private static final String XSD_TARGETNAMESPACE_ATTR = "targetNamespace";

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** COXB package name. */
    public static final String COXB_PACKAGENAME = "coxbPackageName";

    /** JAXB alternate object factory. */
    public static final String COXB_JAXB_ALTERNATIVEFACTORYNAME = "alternativeFactoryName";

    /** JAXB alternate package name. */
    public static final String COXB_JAXB_ALTERNATIVEPACKAGENAME = "alternativePackageName";

    /** Generate JSON Transformers. */
    public static final String COXB_ISJSONTRANSFORMERS = "isJsonTransformers";

    /** Generate XML Transformers. */
    public static final String COXB_ISXMLTRANSFORMERS = "isXmlTransformers";

    /** Compile Transformers. */
    public static final String COXB_IS_COMPILE_TRANSFORMERS = "isCompileTransformers";

    /** XML Schema file. */
    public static final String COXB_XSDFILE = "xsdFile";

    /** JAXB root class names. */
    public static final String COXB_JAXBROOTCLASSNAMES = "jaxbRootClassNames";

    /** JAXB source directory. */
    public static final String COXB_JAXBSRCDIR = "jaxbSrcDir";

    /** JAXB binaries directory. */
    public static final String COXB_JAXBBINDIR = "jaxbBinDir";

    /** COXB source directory. */
    public static final String COXB_COXBSRCDIR = "coxbSrcDir";

    /** COXB binaries directory. */
    public static final String COXB_COXBBINDIR = "coxbBinDir";

    /** Unmarshal choice strategies. */
    public static final String COXB_UNMARSHAL_CHOICE_STRATEGIES = "unmarshalChoiceStrategies";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** JAXB binding customization made available. */
    private JaxbGenModel _jaxbGenModel;

    /** The target package name for generated binding classes. */
    private String _coxbPackageName;

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
    private boolean _xmlTransformers = DEFAULT_ISXMLTRANSFORMERS;

    /** Generate Host to JSON transformers. */
    private boolean _jsonTransformers = DEFAULT_ISJSONTRANSFORMERS;

    /** Compile transformers. */
    private boolean _compileTransformers = DEFAULT_IS_COMPILE_TRANSFORMERS;

    /** The JAXB/COXB annotated XML schema file. */
    private File _xsdFile;

    /** A set of Jaxb root class names to generated binding classes for. */
    private List < String > _jaxbRootClassNames;

    /**
     * The location where JAXB classes sources live. This is not strictly needed
     * for binding generation but is useful when this model is also used for
     * JAXB classes generation.
     */
    private File _jaxbSrcDir;

    /** The location where JAXB compiled classes live. */
    private File _jaxbBinDir;

    /** The target directory where source files will be created. */
    private File _coxbSrcDir;

    /** The target directory where binary files will be created. */
    private File _coxbBinDir;

    /** A list of unmarshal choice strategies to inject in generated bindings. */
    private List < UnmarshalChoiceStrategy > _unmarshalChoiceStrategies;

    /* ====================================================================== */
    /* Following are this class fields that are transient. = */
    /* ====================================================================== */

    /** A general purpose DOM document builder. */
    private DocumentBuilder _docBuilder;

    /**
     * A no-Arg constructor.
     */
    public CoxbGenModel() {
        super();
        _jaxbGenModel = new JaxbGenModel();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public CoxbGenModel(final Properties props) {
        super(props);
        JaxbGenModel xjbModel = new JaxbGenModel(props);
        setJaxbGenModel(xjbModel);
        setCoxbPackageName(getString(props, COXB_PACKAGENAME, null));
        setAlternativePackageName(getString(props,
                COXB_JAXB_ALTERNATIVEPACKAGENAME, null));
        setAlternativeFactoryName(getString(props,
                COXB_JAXB_ALTERNATIVEFACTORYNAME, null));
        setXmlTransformers(getBoolean(props, COXB_ISXMLTRANSFORMERS,
                DEFAULT_ISXMLTRANSFORMERS));
        setJsonTransformers(getBoolean(props, COXB_ISJSONTRANSFORMERS,
                DEFAULT_ISJSONTRANSFORMERS));
        setCompileTransformers(getBoolean(props, COXB_IS_COMPILE_TRANSFORMERS,
                DEFAULT_IS_COMPILE_TRANSFORMERS));
        setXsdFile(getFile(props, COXB_XSDFILE, null));
        setJaxbRootClassNames(getStringList(props, COXB_JAXBROOTCLASSNAMES,
                null));
        setJaxbSrcDir(getFile(props, COXB_JAXBSRCDIR, null));
        setJaxbBinDir(getFile(props, COXB_JAXBBINDIR, null));
        setCoxbSrcDir(getFile(props, COXB_COXBSRCDIR, null));
        setCoxbBinDir(getFile(props, COXB_COXBBINDIR, null));
        setUnmarshalChoiceStrategies(toUnmarshalChoiceStrategies(getStringList(
                props, COXB_UNMARSHAL_CHOICE_STRATEGIES, null)));
    }

    /**
     * Creates an ant build script file ready for binding generation.
     * 
     * @param scriptFile the script file that must be created
     * @throws CodeGenMakeException if generation fails
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
    public void addJaxbRootClassName(final String className) {
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
    public void setXsdFile(final File xsdFile) {
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
        if (_jaxbGenModel.getJaxbPackageName() == null) {
            return getJaxbPackageNameFromXsd(getXsdFile());
        }
        return _jaxbGenModel.getJaxbPackageName();
    }

    /**
     * The package name used for JAXB classes.
     * 
     * @param jaxbPackageName the JAXB classes package name to set
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        _jaxbGenModel.setJaxbPackageName(jaxbPackageName);
    }

    /**
     * @return if IsSet Methods should be generated
     */
    public boolean isGenerateIsSetMethod() {
        return _jaxbGenModel.isGenerateIsSetMethod();
    }

    /**
     * @param generateIsSetMethod if IsSet Methods should be generated
     */
    public void setGenerateIsSetMethod(final boolean generateIsSetMethod) {
        _jaxbGenModel.setGenerateIsSetMethod(generateIsSetMethod);
    }

    /**
     * @return the serialization unique ID. (All JAXB classes must be
     *         serializable for LegStar)
     */
    public long getSerializableUid() {
        return _jaxbGenModel.getSerializableUid();
    }

    /**
     * @param serializableUid the serialization unique ID. (All JAXB classes
     *            must be serializable for LegStar)
     */
    public void setSerializableUid(final long serializableUid) {
        _jaxbGenModel.setSerializableUid(serializableUid);
    }

    /**
     * @return the prefix to add to type names
     */
    public String getTypeNamePrefix() {
        return _jaxbGenModel.getTypeNamePrefix();
    }

    /**
     * @param typeNamePrefix the prefix to add to type names
     */
    public void setTypeNamePrefix(final String typeNamePrefix) {
        _jaxbGenModel.setTypeNamePrefix(typeNamePrefix);
    }

    /**
     * @return the suffix to add to type names
     */
    public String getTypeNameSuffix() {
        return _jaxbGenModel.getTypeNameSuffix();
    }

    /**
     * @param typeNameSuffix the suffix to add to type names
     */
    public void setTypeNameSuffix(final String typeNameSuffix) {
        _jaxbGenModel.setTypeNameSuffix(typeNameSuffix);
    }

    /**
     * @return the prefix to add to element names
     */
    public String getElementNamePrefix() {
        return _jaxbGenModel.getElementNamePrefix();
    }

    /**
     * @param elementNamePrefix the prefix to add to element names
     */
    public void setElementNamePrefix(final String elementNamePrefix) {
        _jaxbGenModel.setElementNamePrefix(elementNamePrefix);
    }

    /**
     * @return the suffix to add to element names
     */
    public String getElementNameSuffix() {
        return _jaxbGenModel.getElementNameSuffix();
    }

    /**
     * @param elementNameSuffix the suffix to add to element names
     */
    public void setElementNameSuffix(final String elementNameSuffix) {
        _jaxbGenModel.setElementNameSuffix(elementNameSuffix);
    }

    /**
     * Whether the ECI naming conventions should be used.
     * 
     * @return Whether the ECI naming conventions should be used
     */
    public boolean isEciCompatible() {
        return _jaxbGenModel.isEciCompatible();
    }

    /**
     * Whether the ECI naming conventions should be used.
     * 
     * @param eciCompatible whether the ECI naming conventions should be used
     */
    public void setEciCompatible(final boolean eciCompatible) {
        _jaxbGenModel.setEciCompatible(eciCompatible);
    }

    /**
     * Prevents generation of JAXB package-info.java which does not compile
     * under JDK 1.5.
     * 
     * @return true if we should not generate package-info.java
     */
    public boolean isNoPackageInfo() {
        return _jaxbGenModel.isNoPackageInfo();
    }

    /**
     * Prevents generation of JAXB package-info.java which does not compile
     * under JDK 1.5.
     * 
     * @param noPackageInfo true if we should not generate package-info.java
     */
    public void setNoPackageInfo(final boolean noPackageInfo) {
        _jaxbGenModel.setNoPackageInfo(noPackageInfo);
    }

    /**
     * @return the package name for generated binding classes
     * @throws CoxbGenException if package name cannot be extracted from XML
     *             Schema
     */
    public String getCoxbPackageName() throws CoxbGenException {
        if (_coxbPackageName == null && getJaxbPackageName() != null) {
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
    public void setAlternativePackageName(final String alternativePackageName) {
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
    public void setAlternativeFactoryName(final String targetFactoryName) {
        _alternativeFactoryName = targetFactoryName;
    }

    /**
     * This is not strictly needed for binding generation but is useful when
     * this model is also used for JAXB classes generation.
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
    public void setJaxbRootClassNames(final List < String > jaxbRootClassNames) {
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
    public JaxbGenModel getJaxbGenModel() {
        return _jaxbGenModel;
    }

    /**
     * @param jaxbGenModel the JAXB binding customization to set
     */
    public void setJaxbGenModel(final JaxbGenModel jaxbGenModel) {
        _jaxbGenModel = jaxbGenModel;
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
     * @return true if Host to JSON transformers generation is turned on
     */
    public boolean isJsonTransformers() {
        return _jsonTransformers;
    }

    /**
     * @param jsonTransformers true if Host to JSON transformers generation is
     *            turned on
     */
    public void setJsonTransformers(final boolean jsonTransformers) {
        _jsonTransformers = jsonTransformers;
    }

    /**
     * @return true if transformers compilation is turned on
     */
    public boolean isCompileTransformers() {
        return _compileTransformers;
    }

    /**
     * @param compileTransformers true if transformers compilation is turned on
     */
    public void setCompileTransformers(final boolean compileTransformers) {
        _compileTransformers = compileTransformers;
    }

    /**
     * Unmarshal choice strategies to inject in generated bindings.
     * 
     * @return the unmarshal choice strategies to inject in generated bindings
     */
    public List < UnmarshalChoiceStrategy > getUnmarshalChoiceStrategies() {
        return _unmarshalChoiceStrategies;
    }

    /**
     * Unmarshal choice strategies to inject in generated bindings.
     * 
     * @param unmarshalChoiceStrategies the unmarshal choice strategies to
     *            inject in generated bindings to set
     */
    public void setUnmarshalChoiceStrategies(
            List < UnmarshalChoiceStrategy > unmarshalChoiceStrategies) {
        this._unmarshalChoiceStrategies = unmarshalChoiceStrategies;
    }

    /**
     * Add a choice strategy to inject in generated bindings.
     * 
     * @param unmarshalChoiceStrategy a choice strategy to inject in generated
     *            bindings
     */
    public void addUnmarshalChoiceStrategy(
            final UnmarshalChoiceStrategy unmarshalChoiceStrategy) {
        if (_unmarshalChoiceStrategies == null) {
            _unmarshalChoiceStrategies = new ArrayList < UnmarshalChoiceStrategy >();
        }
        _unmarshalChoiceStrategies.add(unmarshalChoiceStrategy);
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
                return DEFAULT_JAXB_PACKAGENAME;
            }
            String targetNamespace = ((Element) listOfElements.item(0))
                    .getAttribute(XSD_TARGETNAMESPACE_ATTR);
            if (targetNamespace == null || targetNamespace.length() == 0) {
                return DEFAULT_JAXB_PACKAGENAME;
            }
            return NameUtil.toPackageName(targetNamespace);

        } catch (SAXException e) {
            throw (new CoxbGenException("SAXException " + e.getMessage()));
        } catch (IOException e) {
            throw (new CoxbGenException("IOException " + e.getMessage()));
        }
    }

    /**
     * Turn the JAXB package name into an include statement that we can used as
     * an includes property for a javac ant step.
     * 
     * @return an include statement for javac
     * @throws CoxbGenException if JAXB pacakge name does not exist
     */
    public String getJaxbClassIncludes() throws CoxbGenException {
        String packageName = getJaxbPackageName();
        if (packageName == null || packageName.length() == 0) {
            return packageName;
        }
        String includes = packageName.replace('.', '/');
        return includes + "/**";
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
                DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory
                        .newInstance();
                docBuilderFactory.setNamespaceAware(true);
                _docBuilder = docBuilderFactory.newDocumentBuilder();
            } catch (ParserConfigurationException e) {
                throw new CoxbGenException(e);
            }
        }
        return _docBuilder;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        props.putAll(getJaxbGenModel().toProperties());
        try {
            putString(props, COXB_PACKAGENAME, getCoxbPackageName());
        } catch (CoxbGenException e) {
            putString(props, COXB_PACKAGENAME, e.getMessage());
        }
        putString(props, COXB_JAXB_ALTERNATIVEPACKAGENAME,
                getAlternativePackageName());
        putString(props, COXB_JAXB_ALTERNATIVEFACTORYNAME,
                getAlternativeFactoryName());
        putBoolean(props, COXB_ISXMLTRANSFORMERS, isXmlTransformers());
        putBoolean(props, COXB_ISJSONTRANSFORMERS, isJsonTransformers());
        putBoolean(props, COXB_IS_COMPILE_TRANSFORMERS, isCompileTransformers());
        putFile(props, COXB_XSDFILE, getXsdFile());
        putStringList(props, COXB_JAXBROOTCLASSNAMES, getJaxbRootClassNames());
        putFile(props, COXB_JAXBSRCDIR, getJaxbSrcDir());
        putFile(props, COXB_JAXBBINDIR, getJaxbBinDir());
        putFile(props, COXB_COXBSRCDIR, getCoxbSrcDir());
        putFile(props, COXB_COXBBINDIR, getCoxbBinDir());
        putStringList(props, COXB_UNMARSHAL_CHOICE_STRATEGIES,
                toStringList(getUnmarshalChoiceStrategies()));
        return props;
    }

    /**
     * Helper to serialize the list unmarshal choice strategies to a properties
     * file.
     * 
     * @param unmarshalChoiceStrategies the list of unmarshal choice strategies
     * @return a list of strings
     */
    protected List < String > toStringList(
            List < UnmarshalChoiceStrategy > unmarshalChoiceStrategies) {
        List < String > stringList = new ArrayList < String >();
        if (unmarshalChoiceStrategies != null) {
            for (UnmarshalChoiceStrategy unmarshalChoiceStrategy : unmarshalChoiceStrategies) {
                stringList.add(unmarshalChoiceStrategy.toString());
            }
        }
        return stringList;
    }

    /**
     * Helper to deserialize a list of unmarshal choice strategies from a
     * properties file.
     * 
     * @param stringList a list of strings
     * @return a list of unmarshal choice strategies
     */
    protected List < UnmarshalChoiceStrategy > toUnmarshalChoiceStrategies(
            List < String > stringList) {
        if (stringList != null) {
            List < UnmarshalChoiceStrategy > unmarshalChoiceStrategies = new ArrayList < UnmarshalChoiceStrategy >();
            for (String string : stringList) {
                unmarshalChoiceStrategies.add(new UnmarshalChoiceStrategy(
                        string));
            }
            return unmarshalChoiceStrategies;
        } else {
            return null;
        }
    }

}
