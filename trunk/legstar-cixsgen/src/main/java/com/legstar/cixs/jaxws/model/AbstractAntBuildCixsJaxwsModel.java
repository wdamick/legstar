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
package com.legstar.cixs.jaxws.model;

import java.io.File;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;

/**
 * This model groups parameters needed to generate artifacts for both proxies
 * and adapters.
 */
public abstract class AbstractAntBuildCixsJaxwsModel extends AbstractAntBuildCixsModel {

    /** Target location for web deployment descriptors. */
    private File mTargetWDDDir;

    /** The deployment location for jaxws war files. */
    private File mTargetWarDir;

    /**
     * Construct an empty model.
     * @param generatorName to designate the generator
     * @param vlcTemplate a velocity template that accecpts this model
     */
    public AbstractAntBuildCixsJaxwsModel(
            final String generatorName, final String vlcTemplate) {
        super(generatorName, vlcTemplate);
    }

    /**
     * @return the web service mapping description
     */
    public final CixsJaxwsService getCixsJaxwsService() {
        return (CixsJaxwsService) getCixsService();
    }

    /**
     * @param cixsJaxwsService the web service mapping description to set
     */
    public final void setCixsJaxwsService(
            final CixsJaxwsService cixsJaxwsService) {
        setCixsService(cixsJaxwsService);
    }

    /**
     * @return the Target location for web deployment descriptors
     */
    public final File getTargetWDDDir() {
        return mTargetWDDDir;
    }

    /**
     * @param targetWDDDir the Target location for web deployment descriptors to
     *  set
     */
    public final void setTargetWDDDir(final File targetWDDDir) {
        mTargetWDDDir = targetWDDDir;
    }

    /**
     * @return the deployment location for jaxws war files
     */
    public final File getTargetWarDir() {
        return mTargetWarDir;
    }

    /**
     * @param targetWarDir the deployment location for jaxws war files to set
     */
    public final void setTargetWarDir(final File targetWarDir) {
        mTargetWarDir = targetWarDir;
    }

}
