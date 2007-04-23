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
package com.legstar.xslt;

import java.io.File;
import java.io.InputStream;
import java.util.Vector;

/**
 * This abstract class contains code common to the various generators using
 * XSL transforms.
 * 
 * @author Fady Moussallam
 * 
 */
public abstract class XSLTGenerator {

	/** An instance of the XSL transform engine. */
	private XSLTransform mXformer = new XSLTransform();
	
	/**
	 * No-arg constructor.
	 * 
	 * @throws XSLTException if environment is not setup
	 */
	public XSLTGenerator() throws XSLTException {
	}

	/**
	 * @return the XSL transformer
	 */
	public final XSLTransform getXformer() {
		return mXformer;
	}

	/**
	 * @param xformer the XSL transformer to set
	 */
	public final void setXformer(final XSLTransform xformer) {
		mXformer = xformer;
	}

	/**
	 * Apply an XSLT style sheet transformation without parameters.
	 * @param xmlFileName the descriptor previously generated
	 * @param resultFileName the target location for the generated code
	 * @param styleSheet the style sheet to apply taken frm classpath
	 * @throws XSLTException if transformation fails
	 */
	public final  void transform(
			final String xmlFileName,
			final String resultFileName,
			final String styleSheet) throws XSLTException {
		
		transform(xmlFileName, resultFileName, styleSheet, null);
	}

	/**
	 * Apply an XSLT style sheet transformation.
	 * @param xmlFileName the descriptor previously generated
	 * @param resultFileName the target location for the generated code
	 * @param styleSheet the style sheet to apply taken frm classpath
	 * @param params the parameters expected by the stylesheet
	 * @throws XSLTException if transformation fails
	 */
	public final  void transform(
			final String xmlFileName,
			final String resultFileName,
			final String styleSheet,
			final Vector < XSLTParameter > params) throws XSLTException {
		
		InputStream xsltStream  = getClass().getResourceAsStream(styleSheet);
		if (xsltStream == null) {
			throw (new XSLTException("Unable to locate resource "
					+ styleSheet));
		}
		getXformer().transform(xmlFileName,
				xsltStream,
				new File(resultFileName).toURI().toString(),
				params);
	}
}
