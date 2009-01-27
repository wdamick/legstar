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
package com.legstar.schemagen;

import java.io.File;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * A model for COBOL to COBOL-annotated XML schema.
 */
public class CobolToXsdCobolModel extends SourceToXsdCobolModel {

    /** The full path to the cobol source file. */
    private String mSourceCobolFilePath;

    /** This generator name. */
    public static final String C2S_GENERATOR_NAME =
        "LegStar COBOL to Xsd generator";

    /** This velocity template. */
    public static final String C2S_VELOCITY_MACRO_NAME =
        "vlc/build-cob2xs-xml.vm";

    /**
     * Creates an ant build script file ready for XSD generation.
     * @param targetFile the script file that must be created
     * @throws CodeGenMakeException if generation fails
     */
    public final void generateBuild(
            final File targetFile) throws CodeGenMakeException {
        super.generateBuild(
                C2S_GENERATOR_NAME, C2S_VELOCITY_MACRO_NAME, targetFile);
    }

    /**
     * @return the full path to the COBOL source file
     */
    public final String getSourceCobolFilePath() {
        return mSourceCobolFilePath;
    }

    /**
     * @param sourceCobolFilePath the full path to the COBOL source file to set
     */
    public final void setSourceCobolFilePath(final String sourceCobolFilePath) {
        mSourceCobolFilePath = sourceCobolFilePath;
    }

}
