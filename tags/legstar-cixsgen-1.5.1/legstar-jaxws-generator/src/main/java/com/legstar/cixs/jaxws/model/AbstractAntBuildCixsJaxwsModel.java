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
package com.legstar.cixs.jaxws.model;

import java.io.File;
import java.util.Properties;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;

/**
 * This model groups parameters needed to generate artifacts for both proxies
 * and adapters.
 */
public abstract class AbstractAntBuildCixsJaxwsModel extends
        AbstractAntBuildCixsModel {

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** Target location for web deployment descriptors. */
    public static final String TARGET_WDD_DIR = "targetWDDDir";

    /** The deployment location for jaxws war files. */
    public static final String TARGET_WAR_DIR = "targetWarDir";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** Target location for web deployment descriptors. */
    private File _targetWDDDir;

    /** The deployment location for jaxws war files. */
    private File _targetWarDir;

    /**
     * Construct an empty model.
     * 
     * @param generatorName to designate the generator
     * @param vlcTemplate a velocity template that accecpts this model
     */
    public AbstractAntBuildCixsJaxwsModel(
            final String generatorName, final String vlcTemplate) {
        super(generatorName, vlcTemplate);
        setCixsJaxwsService(new CixsJaxwsService());
    }

    /**
     * Construct from a properties file.
     * 
     * @param generatorName to designate the generator
     * @param vlcTemplate a velocity template that accecpts this model
     * @param props the property file
     */
    public AbstractAntBuildCixsJaxwsModel(final String generatorName,
            final String vlcTemplate, final Properties props) {
        super(generatorName, vlcTemplate, props);
        setTargetWDDDir(getFile(props, TARGET_WDD_DIR, null));
        setTargetWarDir(getFile(props, TARGET_WAR_DIR, null));
        setCixsJaxwsService(new CixsJaxwsService(props));
    }

    /**
     * @return the web service mapping description
     */
    public CixsJaxwsService getCixsJaxwsService() {
        return (CixsJaxwsService) getCixsService();
    }

    /**
     * @param cixsJaxwsService the web service mapping description to set
     */
    public void setCixsJaxwsService(
            final CixsJaxwsService cixsJaxwsService) {
        setCixsService(cixsJaxwsService);
    }

    /**
     * @return the Target location for web deployment descriptors
     */
    public File getTargetWDDDir() {
        return _targetWDDDir;
    }

    /**
     * @param targetWDDDir the Target location for web deployment descriptors to
     *            set
     */
    public void setTargetWDDDir(final File targetWDDDir) {
        _targetWDDDir = targetWDDDir;
    }

    /**
     * @return the deployment location for jaxws war files
     */
    public File getTargetWarDir() {
        return _targetWarDir;
    }

    /**
     * @param targetWarDir the deployment location for jaxws war files to set
     */
    public void setTargetWarDir(final File targetWarDir) {
        _targetWarDir = targetWarDir;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putFile(props, TARGET_WDD_DIR, getTargetWDDDir());
        putFile(props, TARGET_WAR_DIR, getTargetWarDir());
        props.putAll(getCixsJaxwsService().toProperties());
        return props;
    }
}
