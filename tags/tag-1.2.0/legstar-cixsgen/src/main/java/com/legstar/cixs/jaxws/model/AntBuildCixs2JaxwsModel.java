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
package com.legstar.cixs.jaxws.model;

import java.io.File;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;

/**
 * This model groups parameters needed to generate artifacts for an inbound
 * mainframe operation. One such artifacts is a CICS COBOL source that showcases
 * how a remote service can be called from CICS.
 */
public class AntBuildCixs2JaxwsModel extends AbstractAntBuildCixsModel {

    /** This generator name. */
    public static final String CIXS2JAXWS_GENERATOR_NAME =
        "Web Service proxy for Mainframe generator";

	/** Target location for web deployment descriptors. */
	private File mTargetWDDDir;
	
    /** The deployment location for jaxws war files. */
    private File mTargetWarDir;
    
    /** The target directory where COBOL files will be created. */
    private File mTargetCobolDir;
    
	/** This velocity template that creates an ant build which in turn
     * generates the target web service proxy. */
    public static final String CIXS2JAXWS_VELOCITY_MACRO_NAME =
        "vlc/build-cixs2jws-xml.vm";
    
    /**
     * Construct an empty model.
    */
    public AntBuildCixs2JaxwsModel() {
        super(CIXS2JAXWS_GENERATOR_NAME, CIXS2JAXWS_VELOCITY_MACRO_NAME);
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

	/**
	 * @return the directory where COBOL files will be created
	 */
	public final File getTargetCobolDir() {
		return mTargetCobolDir;
	}

	/**
	 * @param targetCobolDir the directory where COBOL files will be created to
	 *  set
	 */
	public final void setTargetCobolDir(final File targetCobolDir) {
		mTargetCobolDir = targetCobolDir;
	}

}
