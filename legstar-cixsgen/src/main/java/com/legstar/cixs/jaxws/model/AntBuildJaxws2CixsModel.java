package com.legstar.cixs.jaxws.model;

import java.io.File;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;

/**
 * This is a model for Jaxws to Cixs component generation. The generated 
 * component runs under Jaxws and wraps a CICS transaction.
 *
 */
public class AntBuildJaxws2CixsModel extends AbstractAntBuildCixsModel {

    /** This generator name. */
    public static final String JAXWS2CIXS_GENERATOR_NAME =
        "Jaxws adapter Web Service generator";

    /** The URI that the host exposes to consumers. */
    private String mHostURI;
    
    /** The host URI is not mandatory at generation time so we
     * provide a placeholder. */
    private static final String DEFAULT_HOST_URI = "http://hosturi";

	/** Target location for web deployment descriptors. */
	private File mTargetWDDDir;
	
    /** The deployment location for jaxws war files. */
    private File mTargetWarDir;
    
   /** This velocity template that creates an ant build which in turn
     * generates the target web service. */
    public static final String JAXWS2CIXS_VELOCITY_MACRO_NAME =
        "vlc/build-m2c-xml.vm";
    
    /**
     * Construct the model.
     */
    public AntBuildJaxws2CixsModel() {
        super(JAXWS2CIXS_GENERATOR_NAME, JAXWS2CIXS_VELOCITY_MACRO_NAME);
    }

    /**
     * @return the URI that the host exposes to consumers
     */
    public final String getHostURI() {
    	if (mHostURI == null || mHostURI.length() == 0) {
    		return DEFAULT_HOST_URI;
    	}
        return mHostURI;
    }

    /**
     * @param hostURI the URI that the host exposes to consumers to set
     */
    public final void setHostURI(final String hostURI) {
        mHostURI = hostURI;
    }

    /**
     * @return the Jaxws web service being generated
     */
    public final CixsJaxwsService getCixsJaxwsServicet() {
        return (CixsJaxwsService) getCixsService();
    }

    /**
     * @param cixsJaxwsService the Jaxws web service being generated
     *  to set
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
