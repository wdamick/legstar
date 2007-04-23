/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.cixs.gen;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.xslt.XSLTException;

import java.io.File;


/**
 * This class generates all artifacts needed for a JAXWS endpoint using
 * COXB cobol binding and CIXS connectivity.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsServiceGenerator extends Task {

	/** Service name. */
	private CixsService mService;
	
	/** Target location for generated source. */
	private String mTargetSrcDir;
	
	/** Target location for ant deployment script. */
	private String mTargetAntDir;
	
	/** Target location for web deployment descriptors. */
	private String mTargetWDDDir;
	
	/** Target location for properties files. */
	private String mTargetPropDir;
	
	/** Target location for web war files. */
	private String mTargetWarDir;
	
	/** Location of jaxb classes binaries. */
	private String mJaxbBinDir;
	
	/** Location of Jaxws endpoint binaries. */
	private String mCixsBinDir;
	
	/** Location of custom binaries. */
	private String mCustBinDir;
	
	/**
	 *  The ant method. The generation process involves the creation of a
	 *  temporary XML file called base descriptor on which a series of XSL
	 *  transforms is applied in order to obtain all the necessary artifacts.
	 */
    public final void execute() {
    	long start = System.currentTimeMillis();
    	
    	try {
    		
    		/* Make sure we have enough valid input*/
    		checkInput();
    		
        	/* Create a set of temporary XML files describing the service and
        	 * its operations to serve as input for XSL transforms. */
			CixsBaseDescriptors gdesc = new CixsBaseDescriptors();
			File sDesc = gdesc.getTempFile();
			gdesc.createServiceContent(mService, sDesc);
			
			/* Generate endpoint code using the temporary base descriptors */
			CixsEndpointSource ep = new CixsEndpointSource();
			ep.createEndpoint(sDesc.getPath(), mTargetSrcDir);
			
			/* Generate web deployment descriptors using the temporary
			 * base descriptors */
			CixsWebDescriptors dp = new CixsWebDescriptors();
			dp.createWebDescriptors(sDesc.getPath(), mTargetWDDDir);
			
			/* Generate an ANT deployment script to help with JAXWS service
			 * deployment. */
			CixsAntDeployment ad = new CixsAntDeployment();
			ad.createDeploymentAnt(sDesc.getPath(),
					mTargetWDDDir,
					mTargetPropDir,
					mTargetAntDir,
					mTargetWarDir,
					mJaxbBinDir,
					mCixsBinDir,
					mCustBinDir);
			
			/* Generate a program properties file for CIXS runtime */
			CixsProgramProp pp = new CixsProgramProp();
			pp.createProgramProp(sDesc.getPath(), mTargetPropDir);
			
		} catch (CixsException e) {
			e.printStackTrace();
			throw (new BuildException(e.getMessage()));
		} catch (XSLTException e) {
			e.printStackTrace();
			throw (new BuildException(e.getMessage()));
		}
		
    	long end = System.currentTimeMillis();
  		System.out.println("Generation success for "
  				+ mService.getServiceName());
    	System.out.println("Duration=" + (end - start) + " ms");
    }
    
	/**
	 * Perform minimal checking over the input values.
	 * @throws CixsException if any of the input is invalid
	 */
    private void checkInput() throws CixsException {
		
    	checkDirectory(mTargetSrcDir);
    	checkDirectory(mTargetWDDDir);
    	checkDirectory(mTargetAntDir);
    	
		if (mService == null) {
			throw (new CixsException("You must provide a service description"));
		}
		if (mTargetWDDDir == null) {
			throw (new CixsException(
					"You must provide a target location for web deployment"
					+ " descriptors"));
		}
		if (mTargetPropDir == null) {
			throw (new CixsException(
					"You must provide a target location for properties files"));
		}
		if (mTargetWarDir == null) {
			throw (new CixsException(
					"You must provide a target location for war files"));
		}
		if (mJaxbBinDir == null) {
			throw (new CixsException(
					"You must provide the location of jaxb binaries"));
		}
		if (mCixsBinDir == null) {
			throw (new CixsException(
				"You must provide the location of jaxws endpoint binaries"));
		}
		if (mCustBinDir == null) {
			throw (new CixsException(
				"You must provide the location of custom binaries"));
		}
    }

	/**
	 * Check that a directory is valid.
	 * @param dir the directory name to check
	 * @throws CixsException if any of the input is invalid
	 */
    private void checkDirectory(final String dir) throws CixsException {
		File fdir = new File(dir);
		
		if (!fdir.exists()) {
			throw (new CixsException(dir + " does not exist"));
		}
		if (!fdir.isDirectory()) {
			throw (new CixsException(dir + " is not a directory"));
		}
		if (!fdir.canWrite()) {
			throw (new CixsException("Directory " + dir 
					+ " is not writable"));
		}
    }
    
	/**
	 * @return the service 
	 */
	public final CixsService getService() {
		return mService;
	}

	/**
	 * @param service the service to set
	 */
	public final void setServiceName(final CixsService service) {
		mService = service;
	}

	/**
	 * @param service the service to set
	 */
	public final void add(final CixsService service) {
		mService = service;
	}

	/**
	 * @param service the service to set
	 */
	public final void addService(final CixsService service) {
		mService = service;
	}

	/**
	 * @return the target source directory
	 */
	public final String getTargetSrcDir() {
		return mTargetSrcDir;
	}

	/**
	 * @param targetSrcDir the target source directory to set
	 */
	public final void setTargetSrcDir(final String targetSrcDir) {
		mTargetSrcDir = targetSrcDir;
	}

	/**
	 * @return the location for web deployment descriptors
	 */
	public final String getTargetWDDDir() {
		return mTargetWDDDir;
	}

	/**
	 * @param targetWDDDir the location for web deployment descriptors
	 *  to set
	 */
	public final void setTargetWDDDir(
			final String targetWDDDir) {
		mTargetWDDDir = targetWDDDir;
	}

	/**
	 * @return the location for ant deployment script
	 */
	public final String getTargetAntDir() {
		return mTargetAntDir;
	}

	/**
	 * @param targetAntDir the location for ant deployment script to set
	 */
	public final void setTargetAntDir(final String targetAntDir) {
		mTargetAntDir = targetAntDir;
	}

	/**
	 * @return the jaxws endpoint binaries
	 */
	public final String getCixsBinDir() {
		return mCixsBinDir;
	}

	/**
	 * @param cixsBinDir the jaxws endpoint binaries to set
	 */
	public final void setCixsBinDir(final String cixsBinDir) {
		mCixsBinDir = cixsBinDir;
	}

	/**
	 * @return the jaxb binaries location
	 */
	public final String getJaxbBinDir() {
		return mJaxbBinDir;
	}

	/**
	 * @param jaxbBinDir the jaxb binaries location to set
	 */
	public final void setJaxbBinDir(final String jaxbBinDir) {
		mJaxbBinDir = jaxbBinDir;
	}

	/**
	 * @return the target war files location
	 */
	public final String getTargetWarDir() {
		return mTargetWarDir;
	}

	/**
	 * @param targetWarDir the target war files location to set
	 */
	public final void setTargetWarDir(final String targetWarDir) {
		mTargetWarDir = targetWarDir;
	}

	/**
	 * @return custom binaries location
	 */
	public final String getCustBinDir() {
		return mCustBinDir;
	}

	/**
	 * @param custBinDir the custom binaries location to set
	 */
	public final void setCustBinDir(final String custBinDir) {
		mCustBinDir = custBinDir;
	}

	/**
	 * @return the target properties files location
	 */
	public final String getTargetPropDir() {
		return mTargetPropDir;
	}

	/**
	 * @param targetPropDir the target properties files location to set
	 */
	public final void setTargetPropDir(final String targetPropDir) {
		mTargetPropDir = targetPropDir;
	}

}
