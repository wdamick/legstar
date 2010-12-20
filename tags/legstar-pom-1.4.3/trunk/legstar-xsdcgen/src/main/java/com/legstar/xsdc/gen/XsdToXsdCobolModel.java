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
package com.legstar.xsdc.gen;

import java.io.File;
import java.net.URI;
import java.util.Properties;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * A model for XML Schema and WSDL to COBOL-annotated XML schema.
 */
public class XsdToXsdCobolModel extends SourceToXsdCobolModel {

    /** This generator name. */
    public static final String X2S_GENERATOR_NAME =
            "LegStar Xsd or Wsdl to Xsd generator";

    /** This velocity template. */
    public static final String X2S_VELOCITY_MACRO_NAME =
            "vlc/build-xsd2xs-xml.vm";

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** URI where the XSD is available. */
    public static final String INPUT_XSD_URI = "inputXsdUri";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** A URI where the XSD is available. */
    private URI _inputXsdUri;

    /**
     * A no-Arg constructor.
     */
    public XsdToXsdCobolModel() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public XsdToXsdCobolModel(final Properties props) {
        super(props);
        setInputXsdUri(getURI(props, INPUT_XSD_URI, null));
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
                X2S_GENERATOR_NAME, X2S_VELOCITY_MACRO_NAME, targetFile);
    }

    /**
     * @return the input XML schema uri
     */
    public URI getInputXsdUri() {
        return _inputXsdUri;
    }

    /**
     * @param xsdUri the input XML schema uri to set
     */
    public void setInputXsdUri(
            final URI xsdUri) {
        _inputXsdUri = xsdUri;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putURI(props, INPUT_XSD_URI, getInputXsdUri());
        return props;
    }
}
