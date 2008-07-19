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
package com.legstar.coxb.common;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolArrayBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;

/**
 * This generic class implements behavior common to all array bindings.  
 *
 */
public abstract class CArrayBinding extends CBinding
		implements ICobolArrayBinding {

	/**
	 * Constructor for a cobol element to java binding.
	 * 
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param parentBinding a reference to the parent binding if any
	 */
	public CArrayBinding(
			final String bindingName,
			final String jaxbName,
			final Class < ? > jaxbType,
			final CobolElement cobolAnnotations,
			final ICobolComplexBinding parentBinding) {
		super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
	}
	
	/** {@inheritDoc} */
	public final int getCurrentOccurs() throws HostException {
		/* If this is a variable size array, ask ancestors for the current
		 * value of the counter we depend on. */
		if (getMinOccurs() < getMaxOccurs() 
				&& getDependingOn() != null
				&& getDependingOn().length() > 0) {
			return getParentBinding().getCounterValue(getDependingOn());
		}
		return this.getMaxOccurs();
	}


}
