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
package com.legstar.j2sc.gen;

import java.io.File;
import java.util.List;
import java.util.Properties;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * A model for Java to COBOL-annotated XML schema.
 */
public class JavaToXsdCobolModel extends SourceToXsdCobolModel {

    /** This velocity template. */
    public static final String J2S_VELOCITY_MACRO_NAME =
            "vlc/build-java2xs-xml.vm";

    /** This generator name. */
    public static final String J2S_GENERATOR_NAME =
            "LegStar Java to Xsd generator";

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** List of fully qualified java class names. */
    public static final String JAVA_CLASS_NAMES = "javaClassNames";

    /** List of classpath elements locations. */
    public static final String JAVA_PATH_ELEMENT_LOCATIONS = "javaPathElementLocations";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** List of fully qualified java class names. */
    private List < String > _javaClassNames;

    /**
     * List of path elements locations to be used as the classpath in
     * order to locate the java classes.
     */
    private List < String > _pathElementLocations;

    /**
     * A no-Arg constructor.
     */
    public JavaToXsdCobolModel() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public JavaToXsdCobolModel(final Properties props) {
        super(props);
        setClassNames(getStringList(props, JAVA_CLASS_NAMES, null));
        setPathElementLocations(getStringList(props,
                JAVA_PATH_ELEMENT_LOCATIONS, null));
    }

    /**
     * Creates an ant build script file ready for XSD generation.
     * 
     * @param targetFile the script file that must be created
     * @throws CodeGenMakeException if generation fails
     */
    public void generateBuild(
            final File targetFile) throws CodeGenMakeException {
        super.generateBuild(
                J2S_GENERATOR_NAME, J2S_VELOCITY_MACRO_NAME, targetFile);
    }

    /**
     * @return the List of fully qualified java class names
     */
    public List < String > getClassNames() {
        return _javaClassNames;
    }

    /**
     * @param classNames the List of fully qualified java class names to set
     */
    public void setClassNames(final List < String > classNames) {
        _javaClassNames = classNames;
    }

    /**
     * @return the List of path elements locations
     */
    public List < String > getPathElementLocations() {
        return _pathElementLocations;
    }

    /**
     * @param pathElementLocations the List of path elements locations to set
     */
    public void setPathElementLocations(
            final List < String > pathElementLocations) {
        _pathElementLocations = pathElementLocations;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putStringList(props, JAVA_CLASS_NAMES, getClassNames());
        putStringList(props, JAVA_PATH_ELEMENT_LOCATIONS,
                getPathElementLocations());
        return props;
    }
}
