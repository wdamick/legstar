/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * A model for XML Schema and WSDL to COBOL-annotated XML schema.
 */
public class XsdToXsdCobolModel extends SourceToXsdCobolModel {

    /** A URI where the XSD is available. */
    private URI mInputXsdUri;

    /** This generator name. */
    public static final String X2S_GENERATOR_NAME =
        "LegStar Xsd or Wsdl to Xsd generator";

    /** This velocity template. */
    public static final String X2S_VELOCITY_MACRO_NAME =
        "vlc/build-xsd2xs-xml.vm";

    /**
     * Creates an ant build script file ready for XSD generation.
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
    public final URI getInputXsdUri() {
        return mInputXsdUri;
    }

    /**
     * @param xsdUri the input XML schema uri to set
     */
    public final void setInputXsdUri(
            final URI xsdUri) {
        mInputXsdUri = xsdUri;
    }

}