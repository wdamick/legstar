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
 * This class contains the logic to generate a JAXWS endpoint code using
 * previously generated descriptors and a set of XSL transforms.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsEndpointSource {

	/** The XSL transform for service endpoint interface. */
	private static final String  SEI_STYLE = "/xslt/sei.xsl";
	
	/** The XSL transform for service endpoint implementation. */
	private static final String  SEIMPL_STYLE = "/xslt/sei-impl.xsl";
	
	/** The XSL transform for host header implementation. */
	private static final String  HOSTHEADER_STYLE = "/xslt/sei-hostheader.xsl";
	
	/** The XSL transform for service endpoint operations. */
	private static final String  OPERATION_STYLE = "/xslt/sei-operation.xsl";
	
	/** Endpoint interface XSL stylesheet associated with this transformer. */
	private static XSLTransform mTransformerEI;
	
	/** Endpoint implementation XSL stylesheet associated with this
	 *  transformer. */
	private static XSLTransform mTransformerEX;
	
	/** Header implementation XSL stylesheet associated with this
	 *  transformer. */
	private static XSLTransform mTransformerHI;
	
	/** Endpoint operations XSL stylesheet associated with this transformer. */
	private static XSLTransform mTransformerEO;
	
	/**
	 * No-arg constructor.
	 * 
	 * @throws XSLTException if environment is not setup
	 */
	public CixsEndpointSource() throws XSLTException {
		if (mTransformerEI == null) {
			InputStream xsltStream  = getClass().getResourceAsStream(
					SEI_STYLE);
			mTransformerEI = new XSLTransform(xsltStream);
		}
		if (mTransformerEX == null) {
			InputStream xsltStream  = getClass().getResourceAsStream(
					SEIMPL_STYLE);
			mTransformerEX = new XSLTransform(xsltStream);
		}
		if (mTransformerHI == null) {
			InputStream xsltStream  = getClass().getResourceAsStream(
					HOSTHEADER_STYLE);
			mTransformerHI = new XSLTransform(xsltStream);
		}
		if (mTransformerEO == null) {
			InputStream xsltStream  = getClass().getResourceAsStream(
					OPERATION_STYLE);
			mTransformerEO = new XSLTransform(xsltStream);
		}
	}
	
	/**
	 * Create enpoint code.
	 * 
	 * @param serviceDescriptorFile the descriptor previously generated
	 * @param targetDir the target location for the generated code
	 * @throws XSLTException if transformation fails
	 */
	public final void createEndpoint(
			final String serviceDescriptorFile,
			final String targetDir) throws XSLTException {

		String targetFileName = targetDir + '/' + "dummy";
		/* Create service endpoint */
		mTransformerEI.transform(serviceDescriptorFile,
				new File(targetFileName).toURI().toString());
		
		/* Create service endpoint implementation */
		mTransformerEX.transform(serviceDescriptorFile,
				new File(targetFileName).toURI().toString());

		/* Create host header */
		mTransformerHI.transform(serviceDescriptorFile,
				new File(targetFileName).toURI().toString());

		/* Create service endpoint faults and request/response wrappers */
		mTransformerEO.transform(serviceDescriptorFile,
				new File(targetFileName).toURI().toString());
	}

}
