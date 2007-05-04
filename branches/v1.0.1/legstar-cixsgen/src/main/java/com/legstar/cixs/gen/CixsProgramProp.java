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

import com.legstar.xslt.XSLTException;
import com.legstar.xslt.XSLTransform;

/**
 * This class contains the logic to generate a program properties file using
 * previously generated descriptors and an XSL transform.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsProgramProp {

	/** The XSL transform for program properties. */
	private static final String  PROGPROP_STYLE = "/xslt/program-prop.xsl";

	/** Program properties XSL stylesheet associated with this transformer. */
	private static XSLTransform mTransformer;
	
	/**
	 * No-arg constructor.
	 * 
	 * @throws XSLTException if environment is not setup
	 */
	public CixsProgramProp() throws XSLTException {
		if (mTransformer == null) {
			/* XSLT style sheet is a resource within the jar */
			InputStream xsltStream  = getClass().getResourceAsStream(
					PROGPROP_STYLE);
			mTransformer = new XSLTransform(xsltStream);
		}
	}
	
	/**
	 * Create a program properties file for each operation in a service.
	 * 
	 * @param serviceDescriptorFile the descriptor previously generated
	 * @param targetDir the target location for the program properties
	 * @throws XSLTException if XSLT transform fails
	 */
	public final void createProgramProp(
			final String serviceDescriptorFile,
			final String targetDir) throws XSLTException {
		
		String targetFileName = targetDir + '/' + "dummy";
		mTransformer.setParams(null);
		mTransformer.transform(serviceDescriptorFile,
				new File(targetFileName).toURI().toString());
	}
}
