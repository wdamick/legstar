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
package com.legstar.coxb.common;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;

/**
 * This class is a superclass of all arrays of complex element types
 * implementing binding between a java type and cobol.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CArrayComplexBinding extends CArrayBinding
	implements ICobolArrayComplexBinding {

	/** Reference to the complex binding representing each occurence of this 
	 * array. */
	private ICobolComplexBinding mComplexItemBinding;

	/**
	 * Constructor for a cobol element to java binding.
	 * 
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param parentBinding a reference to the parent binding if any
	 * @param complexItemBinding a binding element for array items
	 */
	public CArrayComplexBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations,
			final ICobolComplexBinding parentBinding,
			final ICobolComplexBinding complexItemBinding) {
		super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
		mComplexItemBinding = complexItemBinding;
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}

	/** {@inheritDoc} */
	public final int calcByteLength() throws HostException {
		return getMaxOccurs() * mComplexItemBinding.calcByteLength();
	}
	
	/**
	 * @return the Complex Binding describing an item
	 */
	public final ICobolComplexBinding getComplexItemBinding() {
		return mComplexItemBinding;
	}

	/**
	 * @param complexItemBinding the item Complex Binding to set
	 */
	public final void setComplexItemBinding(
			final ICobolComplexBinding complexItemBinding) {
		mComplexItemBinding = complexItemBinding;
	}

}
