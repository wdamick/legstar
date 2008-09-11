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
package com.legstar.coxb.impl;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolComplexBinding;

/**
 * This class implements the behavior of an array of national cobol elements
 * bound to a JAXB String property.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayNationalBinding	extends CArrayStringBinding
	implements ICobolArrayNationalBinding {
	
	/**
	 * Constructor for a cobol element to java binding.
	 * 
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param parentBinding a reference to the parent binding if any
	 */
	public CArrayNationalBinding(
			final String bindingName,
			final String jaxbName,
			final Class < ? > jaxbType,
			final CobolElement cobolAnnotations,
			final ICobolComplexBinding parentBinding) {
		super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
	}
	
}
