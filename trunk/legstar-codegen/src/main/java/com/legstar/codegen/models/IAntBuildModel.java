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
package com.legstar.codegen.models;

import java.io.File;

import com.legstar.codegen.CodeGenMakeException;

/**
 * Generators that create ant builds will implement this interface.
 *
 */
public interface IAntBuildModel {

    /**
     * Create an ant build script ready to run.
     * @param scriptFile the build file
     * @throws CodeGenMakeException if build cannot be generated
     */
    void generateBuild(File scriptFile) throws CodeGenMakeException;

    /**
     * @return the temporary file used to check whether the ant script succeeded
     */
    File getProbeFile();

    /**
     * @param probeFile the temporary file used to check whether the ant
     *  script succeeded to set
     */
    void setProbeFile(final File probeFile);
}
