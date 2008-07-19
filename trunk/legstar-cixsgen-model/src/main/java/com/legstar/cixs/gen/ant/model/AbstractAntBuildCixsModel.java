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
package com.legstar.cixs.gen.ant.model;

import java.io.File;

import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractAntBuildModel;

/**
 * Holds the set of parameters needed to generate an ant script file.
 * This can be used by Eclipse plugins to generate ant files.
 * This model is common to all generation types.
 */
public class AbstractAntBuildCixsModel extends AbstractAntBuildModel {
    
    /** Describes the service being generated. */
    private AbstractCixsService mCixsService;
    
    /** The target directory where source files will be created. */
    private File mTargetSrcDir;
    
    /** The target directory where binary files will be created. */
    private File mTargetBinDir;
    
    /** The target directory where ant files will be created. */
    private File mTargetAntDir;
    
    /** The target directory where properties files will be created. */
    private File mTargetPropDir;
    
    /** The location where JAXB compiled classes live. */
    private File mJaxbBinDir;
    
    /** The location where COXB compiled classes live. */
    private File mCoxbBinDir;
    
    /** The location where custom compiled classes live. */
    private File mCustBinDir;
    
    /** The host character set. Default is US 37 + Euro sign. */
    private String mHostCharset = "IBM01140";
    
    /** This generator's name. */
    private String mGeneratorName;
    
    /** This velocity template. */
    private String mVlcTemplate;
    
    /**
     * Construct the model with a generator name and velocity template.
     * @param generatorName to designate the generator
     * @param vlcTemplate a velocity template that accepts this model
     */
    public AbstractAntBuildCixsModel(
            final String generatorName, final String vlcTemplate) {
        mVlcTemplate = vlcTemplate;
        mGeneratorName = generatorName;
    }
    
    /** {@inheritDoc} */
    public void generateBuild(
    		final File scriptFile) throws CodeGenMakeException {
        super.generateBuild(
                mGeneratorName, mVlcTemplate, scriptFile);
    }

   /**
     * @return the target directory where source files will be created
     */
    public final File getTargetSrcDir() {
        return mTargetSrcDir;
    }

    /**
     * @param targetSrcDir the target directory where source files will be
     *  created to set
     */
    public final void setTargetSrcDir(final File targetSrcDir) {
        mTargetSrcDir = targetSrcDir;
    }

    /**
     * @return the target directory where binary files will be created
     */
    public final File getTargetBinDir() {
        return mTargetBinDir;
    }

    /**
     * @param targetBinDir the target directory where binary files will be
     *  created to set
     */
    public final void setTargetBinDir(final File targetBinDir) {
        mTargetBinDir = targetBinDir;
    }

    /**
     * @return the target directory where ant files will be created
     */
    public final File getTargetAntDir() {
        return mTargetAntDir;
    }

    /**
     * @param targetAntDir the target directory where ant files will be created
     *  to set
     */
    public final void setTargetAntDir(final File targetAntDir) {
        mTargetAntDir = targetAntDir;
    }

    /**
     * @return the target directory where properties files will be created
     */
    public final File getTargetPropDir() {
        return mTargetPropDir;
    }

    /**
     * @param targetPropDir the target directory where properties files will be
     *  created to set
     */
    public final void setTargetPropDir(final File targetPropDir) {
        mTargetPropDir = targetPropDir;
    }

    /**
     * @return the location where JAXB compiled classes live
     */
    public final File getJaxbBinDir() {
        return mJaxbBinDir;
    }

    /**
     * @param jaxbBinDir the location where JAXB compiled classes live to set
     */
    public final void setJaxbBinDir(final File jaxbBinDir) {
        mJaxbBinDir = jaxbBinDir;
    }

    /**
     * @return the location where COXB compiled classes live. Defaults to same
     * location as Jaxb classes.
     */
    public final File getCoxbBinDir() {
    	if (mCoxbBinDir == null) {
    		return getJaxbBinDir();
    	}
        return mCoxbBinDir;
    }

    /**
     * @param coxbBinDir the location where COXB compiled classes live to set
     */
    public final void setCoxbBinDir(final File coxbBinDir) {
        mCoxbBinDir = coxbBinDir;
    }

    /**
     * @return the location where custom compiled classes live. Defaults to same
     * location as Jaxb classes.
     */
    public final File getCustBinDir() {
    	if (mCustBinDir == null) {
    		return getJaxbBinDir();
    	}
        return mCustBinDir;
    }

    /**
     * @param custBinDir the location where custom compiled classes live to set
     */
    public final void setCustBinDir(final File custBinDir) {
        mCustBinDir = custBinDir;
    }

    /**
     * @return this generator's name
     */
    public final String getGeneratorName() {
        return mGeneratorName;
    }

    /**
     * @param generatorName this generator's name to set
     */
    public final void setGeneratorName(final String generatorName) {
        mGeneratorName = generatorName;
    }

    /**
     * @return the host character set
     */
    public final String getHostCharset() {
        return mHostCharset;
    }

    /**
     * @param hostCharset the host character set to set
     */
    public final void setHostCharset(final String hostCharset) {
        mHostCharset = hostCharset;
    }

    /**
     * @return the the service being generated
     */
    public final AbstractCixsService getCixsService() {
        return mCixsService;
    }

    /**
     * @param cixsService the service being generated  to set
     */
    public final void setCixsService(
            final AbstractCixsService cixsService) {
        mCixsService = cixsService;
    }


}
