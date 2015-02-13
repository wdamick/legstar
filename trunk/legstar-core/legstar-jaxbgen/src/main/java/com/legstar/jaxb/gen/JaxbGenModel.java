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
package com.legstar.jaxb.gen;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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

    /** Default value for no package-info generation. */
    public static final boolean DEFAULT_NOPACKAGEINFO = false;

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

    /** Don't generate package-info.java. */
    public static final String JAXB_NOPACKAGEINFO = "noPackageInfo";

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

    /** Whether we should not generate package-info.java. */
    private boolean _noPackageInfo = DEFAULT_NOPACKAGEINFO;

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
        setNoPackageInfo(getBoolean(props, JAXB_NOPACKAGEINFO,
                DEFAULT_NOPACKAGEINFO));
    }

    /**
     * Creates an external binding customization file ready for JAXB.
     * <p/>
     * This external binding file has less capabilities than the alternative
     * which is to inject annotations inline in the XML schema but has the
     * advantage of not messing up with the original XML schema.
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
     * Prevents generation of JAXB package-info.java which does not compile
     * under JDK 1.5.
     * 
     * @return true if we should not generate package-info.java
     */
    public boolean isNoPackageInfo() {
        return _noPackageInfo;
    }

    /**
     * Prevents generation of JAXB package-info.java which does not compile
     * under JDK 1.5.
     * 
     * @param noPackageInfo true if we should not generate package-info.java
     */
    public void setNoPackageInfo(final boolean noPackageInfo) {
        this._noPackageInfo = noPackageInfo;
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
        putBoolean(props, JAXB_NOPACKAGEINFO, isNoPackageInfo());
        return props;
    }

}
