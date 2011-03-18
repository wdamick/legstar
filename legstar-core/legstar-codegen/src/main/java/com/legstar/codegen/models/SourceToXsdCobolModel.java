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
package com.legstar.codegen.models;

import java.io.File;
import java.util.Properties;

/**
 * A model usable for XML Schema generation.
 * Groups all the data needed to generate an annotated XML schema from
 * a source. This is primarily intended to support velocity
 * template generation of ad-hoc ant scripts.
 * Also provides a convenience method to help with velocity generation.
 * 
 */
public abstract class SourceToXsdCobolModel extends AbstractAntBuildModel {

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** XSD target namespace. */
    public static final String XSD_TARGET_NAMESPACE = "targetNamespace";

    /** XSD target directory. */
    public static final String XSD_TARGET_DIR = "xsdTargetDir";

    /** XSD target file name. */
    public static final String XSD_TARGET_FILENAME = "targetXsdFileName";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** The target schema namespace. */
    private String _targetNamespace = "";

    /** The target directory where annotated XSD will be created. */
    private File _xsdTargetDir;

    /** The target annotated XSD file name. */
    private String _targetXsdFileName;

    /**
     * A no-Arg constructor.
     */
    public SourceToXsdCobolModel() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public SourceToXsdCobolModel(final Properties props) {
        super(props);
        setNamespace(getString(props, XSD_TARGET_NAMESPACE, null));
        setTargetDir(getFile(props, XSD_TARGET_DIR, null));
        setTargetXsdFileName(getString(props, XSD_TARGET_FILENAME, null));
    }

    /**
     * @return the The target schema namespace
     */
    public String getNamespace() {
        return _targetNamespace;
    }

    /**
     * @param namespace the The target schema namespace to set
     */
    public void setNamespace(final String namespace) {
        if (namespace != null) {
            /*
             * Keep namespacelowercase so we can derive package names
             * from it if necessary.
             */
            _targetNamespace = namespace.toLowerCase();
        } else {
            _targetNamespace = null;
        }
    }

    /**
     * @return the The target directory
     */
    public File getTargetDir() {
        return _xsdTargetDir;
    }

    /**
     * @param targetDir the The target directory to set
     */
    public void setTargetDir(final File targetDir) {
        _xsdTargetDir = targetDir;
    }

    /**
     * @return the The target annotated XSD file name
     */
    public String getTargetXsdFileName() {
        return _targetXsdFileName;
    }

    /**
     * @param targetXsdFileName the The target annotated XSD file name to set
     */
    public void setTargetXsdFileName(final String targetXsdFileName) {
        _targetXsdFileName = targetXsdFileName;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putString(props, XSD_TARGET_NAMESPACE, getNamespace());
        putFile(props, XSD_TARGET_DIR, getTargetDir());
        putString(props, XSD_TARGET_FILENAME, getTargetXsdFileName());
        return props;
    }
}
