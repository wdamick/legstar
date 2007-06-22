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

import java.io.File;
import java.io.InputStream;
import java.util.Vector;

import com.legstar.xslt.XSLTException;
import com.legstar.xslt.XSLTParameter;
import com.legstar.xslt.XSLTransform;

/**
 * This class contains the logic to generate an ANT deployment script using
 * previously generated descriptors and a set of XSL transforms.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsAntDeployment {

	/** The XSL transform for ant deployment script. */
	private static final String  BUILD_STYLE = "/xslt/ant-build-war.xsl";
	
	/** The resulting ant deployment script. */
	private static final String  BUILD_FILE = "build.xml";
	
	/** The XSLT parameter for target deployment descriptor location. */
	private static final String  TARGET_WDD_DIR = "wdd-dir";
	
	/** The XSLT parameter for target properties files location. */
	private static final String  TARGET_PROP_DIR = "prop-dir";
	
	/** The XSLT parameter for target war location. */
	private static final String  TARGET_WAR_DIR = "war-dir";
	
	/** The XSLT parameter for jaxb binaries location. */
	private static final String  JAXB_BIN_DIR = "jaxb-bin-dir";
	
	/** The XSLT parameter for coxb binaries location. */
	private static final String  COXB_BIN_DIR = "coxb-bin-dir";
	
	/** The XSLT parameter for jaxws endpoint binaries location. */
	private static final String  CIXS_BIN_DIR = "cixs-bin-dir";
	
	/** The XSLT parameter for custom binaries location. */
	private static final String  CUST_BIN_DIR = "cust-bin-dir";
	
	/** ANT deployment XSL stylesheet associated with this transformer. */
	private static XSLTransform mTransformer;
	
	/**
	 * No-arg constructor.
	 * 
	 * @throws XSLTException if environment is not setup
	 */
	public CixsAntDeployment() throws XSLTException {
		if (mTransformer == null) {
			/* XSLT style sheet is a resource within the jar */
			InputStream xsltStream  = getClass().getResourceAsStream(
					BUILD_STYLE);
			mTransformer = new XSLTransform(xsltStream);
		}
	}
	
	/**
	 * Create deployment ANT script.
	 * 
	 * @param serviceDescriptorFile the descriptor previously generated
	 * @param targetWddDir the target location for web deployment descriptors
	 * @param targetPropDir the target location for properties files
	 * @param targetAntDir the target location for the ant script
	 * @param targetWarDir the target location for war files
	 * @param jaxbBinDir the location for jaxb binaries
	 * @param coxbBinDir the location for coxb binaries
	 * @param cixsBinDir the location for jaxws endpoint binaries
	 * @param custBinDir the location for custom binaries
	 * @throws XSLTException if XSLT transform fails
	 */
	public final void createDeploymentAnt(
			final String serviceDescriptorFile,
			final String targetWddDir,
			final String targetPropDir,
		    final String targetAntDir,
		    final String targetWarDir,
		    final String jaxbBinDir,
		    final String coxbBinDir,
		    final String cixsBinDir,
		    final String custBinDir) throws XSLTException {

		Vector < XSLTParameter > params = new Vector < XSLTParameter >();

		XSLTParameter targetWddDirParm = new XSLTParameter();
		targetWddDirParm.setName(TARGET_WDD_DIR);
		targetWddDirParm.setExpression(targetWddDir);
		params.add(targetWddDirParm);

		XSLTParameter targetPropDirParm = new XSLTParameter();
		targetPropDirParm.setName(TARGET_PROP_DIR);
		targetPropDirParm.setExpression(targetPropDir);
		params.add(targetPropDirParm);

		XSLTParameter targetWarDirParm = new XSLTParameter();
		targetWarDirParm.setName(TARGET_WAR_DIR);
		targetWarDirParm.setExpression(targetWarDir);
		params.add(targetWarDirParm);

		XSLTParameter jaxbBinDirParm = new XSLTParameter();
		jaxbBinDirParm.setName(JAXB_BIN_DIR);
		jaxbBinDirParm.setExpression(jaxbBinDir);
		params.add(jaxbBinDirParm);
		
		XSLTParameter coxbBinDirParm = new XSLTParameter();
		coxbBinDirParm.setName(COXB_BIN_DIR);
		coxbBinDirParm.setExpression(coxbBinDir);
		params.add(coxbBinDirParm);
		
		XSLTParameter cixsBinDirParm = new XSLTParameter();
		cixsBinDirParm.setName(CIXS_BIN_DIR);
		cixsBinDirParm.setExpression(cixsBinDir);
		params.add(cixsBinDirParm);
		
		XSLTParameter custBinDirParm = new XSLTParameter();
		custBinDirParm.setName(CUST_BIN_DIR);
		custBinDirParm.setExpression(custBinDir);
		params.add(custBinDirParm);
		
		mTransformer.setParams(params);
		mTransformer.transform(serviceDescriptorFile,
				new File(targetAntDir + '/' + BUILD_FILE).toURI().toString());
	}

}
