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
package com.legstar.codegen.models;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.codegen.CodeGenVelocityException;

/**
 * A model usable for ant script generation. Meant to be used by
 * specialized generators which first create and ant script which
 * in turn creates the various artifacts.
 *
 */
public abstract class AbstractAntBuildModel implements IAntBuildModel {

    /** Where the base LegStar product is installed. */
    private String mProductLocation;

    /** This temporary file can be used by caller to check whether the ant
     * script succeeded. */
    private File mProbeFile;

    /**  Suffix of generated ANT script file. */
    public static final String ANT_FILE_SUFFIX = "xml";

    /** Prefix of generated ANT script file. */
    public static final String ANT_FILE_PREFIX = "build-";

    /** Set of parameters to be passed to velocity.*/
    private Map < String, Object > mParameters;

    /**
     * Creates an ant build script file ready for launching.
     * @param generatorName the name of the actual generator
     * @param targetFile the script file that must be created
     * @param velocityMacro the template to apply
     * @throws CodeGenMakeException if generation fails
     */
    public void generateBuild(
            final String generatorName,
            final String velocityMacro,
            final File targetFile) throws CodeGenMakeException {
        try {
            if (mParameters == null) {
                CodeGenUtil.initVelocity();
                CodeGenHelper helper = new CodeGenHelper();
                mParameters = new HashMap < String, Object >();
                mParameters.put("helper", helper);
            }

            CodeGenUtil.processTemplate(
                    generatorName,
                    velocityMacro,
                    "antModel", this,
                    mParameters,
                    targetFile);

        } catch (CodeGenVelocityException e) {
            throw new CodeGenMakeException(e);
        }
    }

    /**
     * @return the Where the base LegStar product is installed. This is used
     * as the basedir for the generated ant script.
     */
    public String getProductLocation() {
        return mProductLocation;
    }

    /**
     * @param productLocation the Where the base LegStar product is installed to
     *  set
     */
    public void setProductLocation(final String productLocation) {
        mProductLocation = productLocation;
    }

    /**
     * @return the temporary file used to check whether the ant script succeeded
     */
    public File getProbeFile() {
        return mProbeFile;
    }

    /**
     * @param probeFile the temporary file used to check whether the ant
     *  script succeeded to set
     */
    public void setProbeFile(final File probeFile) {
        mProbeFile = probeFile;
    }
}
