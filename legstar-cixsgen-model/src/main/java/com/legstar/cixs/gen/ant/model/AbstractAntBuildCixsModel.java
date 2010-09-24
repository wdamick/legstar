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
package com.legstar.cixs.gen.ant.model;

import java.io.File;
import java.util.Properties;

import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractAntBuildModel;

/**
 * Holds the set of parameters needed to generate an ant script file.
 * This can be used by Eclipse plugins to generate ant files.
 * This model is common to all generation types.
 */
public abstract class AbstractAntBuildCixsModel extends AbstractAntBuildModel {

    /*
     * Following are key identifiers for this model serialization.
     */

    /** Target source directory. */
    public static final String TARGET_SRC_DIR = "targetSrcDir";

    /** Target binaries directory. */
    public static final String TARGET_BIN_DIR = "targetBinDir";

    /** Target ANT directory. */
    public static final String TARGET_ANT_DIR = "targetAntDir";

    /** Target distribution directory. */
    public static final String TARGET_DIST_DIR = "targetDistDir";

    /** JAXB binaries directory. */
    public static final String JAXB_BIN_DIR = "jaxbBinDir";

    /** COXB binaries directory. */
    public static final String COXB_BIN_DIR = "coxbBinDir";

    /** Custom binaries directory. */
    public static final String CUST_BIN_DIR = "custBinDir";

    /** Host character set. */
    public static final String HOST_CHARSET = "hostCharset";

    /*
     * Following are this class fields that are persistent.
     */

    /** The target directory where source files will be created. */
    private File _targetSrcDir;

    /** The target directory where binary files will be created. */
    private File _targetBinDir;

    /** The target directory where ant files will be created. */
    private File _targetAntDir;

    /** The distribution location for artifacts such as jars and wars. */
    private File _targetDistDir;

    /** The location where JAXB compiled classes live. */
    private File _jaxbBinDir;

    /** The location where COXB compiled classes live. */
    private File _coxbBinDir;

    /** The location where custom compiled classes live. */
    private File _custBinDir;

    /** The host character set. Default is US 37 + Euro sign. */
    private String _hostCharset = "IBM01140";

    /*
     * Following are this class fields that are transient.
     */

    /** Describes the service being generated. */
    private AbstractCixsService _cixsService;

    /** This generator's name. */
    private String _generatorName;

    /** This velocity template. */
    private String _vlcTemplate;

    /**
     * A no-Arg constructor.
     */
    public AbstractAntBuildCixsModel() {
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public AbstractAntBuildCixsModel(final Properties props) {
        setTargetSrcDir(getFile(props, TARGET_SRC_DIR, null));
        setTargetBinDir(getFile(props, TARGET_BIN_DIR, null));
        setTargetAntDir(getFile(props, TARGET_ANT_DIR, null));
        setTargetDistDir(getFile(props, TARGET_DIST_DIR, null));
        setJaxbBinDir(getFile(props, JAXB_BIN_DIR, null));
        setCoxbBinDir(getFile(props, COXB_BIN_DIR, null));
        setCustBinDir(getFile(props, CUST_BIN_DIR, null));
        setHostCharset(getString(props, HOST_CHARSET, null));
    }

    /**
     * Construct the model with a generator name and velocity template.
     * 
     * @param generatorName to designate the generator
     * @param vlcTemplate a velocity template that accepts this model
     */
    public AbstractAntBuildCixsModel(
            final String generatorName, final String vlcTemplate) {
        _vlcTemplate = vlcTemplate;
        _generatorName = generatorName;
    }

    /** {@inheritDoc} */
    public void generateBuild(
            final File scriptFile) throws CodeGenMakeException {
        super.generateBuild(
                _generatorName, _vlcTemplate, scriptFile, "UTF-8");
    }

    /**
     * @return the target directory where source files will be created
     */
    public File getTargetSrcDir() {
        return _targetSrcDir;
    }

    /**
     * @param targetSrcDir the target directory where source files will be
     *            created to set
     */
    public void setTargetSrcDir(final File targetSrcDir) {
        _targetSrcDir = targetSrcDir;
    }

    /**
     * @return the target directory where binary files will be created
     */
    public File getTargetBinDir() {
        return _targetBinDir;
    }

    /**
     * @param targetBinDir the target directory where binary files will be
     *            created to set
     */
    public void setTargetBinDir(final File targetBinDir) {
        _targetBinDir = targetBinDir;
    }

    /**
     * @return the target directory where ant files will be created
     */
    public File getTargetAntDir() {
        return _targetAntDir;
    }

    /**
     * @param targetAntDir the target directory where ant files will be created
     *            to set
     */
    public void setTargetAntDir(final File targetAntDir) {
        _targetAntDir = targetAntDir;
    }

    /**
     * @return the distribution location for artifacts such as jars and wars
     */
    public File getTargetDistDir() {
        return _targetDistDir;
    }

    /**
     * @param targetDistDir the distribution location for artifacts such as jars
     *            and wars to set
     */
    public void setTargetDistDir(final File targetDistDir) {
        _targetDistDir = targetDistDir;
    }

    /**
     * @return the location where JAXB compiled classes live
     */
    public File getJaxbBinDir() {
        return _jaxbBinDir;
    }

    /**
     * @param jaxbBinDir the location where JAXB compiled classes live to set
     */
    public void setJaxbBinDir(final File jaxbBinDir) {
        _jaxbBinDir = jaxbBinDir;
    }

    /**
     * @return the location where COXB compiled classes live. Defaults to same
     *         location as Jaxb classes.
     */
    public File getCoxbBinDir() {
        if (_coxbBinDir == null) {
            return getJaxbBinDir();
        }
        return _coxbBinDir;
    }

    /**
     * @param coxbBinDir the location where COXB compiled classes live to set
     */
    public void setCoxbBinDir(final File coxbBinDir) {
        _coxbBinDir = coxbBinDir;
    }

    /**
     * @return the location where custom compiled classes live. Defaults to same
     *         location as Jaxb classes.
     */
    public File getCustBinDir() {
        if (_custBinDir == null) {
            return getJaxbBinDir();
        }
        return _custBinDir;
    }

    /**
     * @param custBinDir the location where custom compiled classes live to set
     */
    public void setCustBinDir(final File custBinDir) {
        _custBinDir = custBinDir;
    }

    /**
     * @return this generator's name
     */
    public String getGeneratorName() {
        return _generatorName;
    }

    /**
     * @param generatorName this generator's name to set
     */
    public void setGeneratorName(final String generatorName) {
        _generatorName = generatorName;
    }

    /**
     * @return the host character set
     */
    public String getHostCharset() {
        return _hostCharset;
    }

    /**
     * @param hostCharset the host character set to set
     */
    public void setHostCharset(final String hostCharset) {
        _hostCharset = hostCharset;
    }

    /**
     * @return the the service being generated
     */
    public AbstractCixsService getCixsService() {
        return _cixsService;
    }

    /**
     * @param cixsService the service being generated to set
     */
    public void setCixsService(
            final AbstractCixsService cixsService) {
        _cixsService = cixsService;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = new Properties();
        putFile(props, TARGET_SRC_DIR, getTargetSrcDir());
        putFile(props, TARGET_BIN_DIR, getTargetBinDir());
        putFile(props, TARGET_ANT_DIR, getTargetAntDir());
        putFile(props, TARGET_DIST_DIR, getTargetDistDir());
        putFile(props, JAXB_BIN_DIR, getJaxbBinDir());
        putFile(props, COXB_BIN_DIR, getCoxbBinDir());
        putFile(props, CUST_BIN_DIR, getCustBinDir());
        putString(props, HOST_CHARSET, getHostCharset());
        return props;
    }
}
