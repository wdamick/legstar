/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.coxb.rt;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.host.HostException;

/**
 * This class is a superclass of all arrays of complex element types
 * implementing binding between a java type and cobol.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CArrayComplexBinding
	extends CArrayBinding implements ICobolArrayComplexBinding {

	/** Reference to the complex binding representing each occurence of this 
	 * array. */
	private CComplexBinding mComplexBinding;

	/**
	 * Constructor for a cobol array of simple elements to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param parentBinding a reference to the parent binding if any
	 * @param complexBinding complex binding representing each occurence
	 * @param minOccurs minimum number of occurences
	 * @param maxOccurs maximum number of occurences
	 */
	public CArrayComplexBinding(
			final String javaName,
			final String javaType,
			final CComplexBinding parentBinding,
			final CComplexBinding complexBinding,
			final int minOccurs,
			final int maxOccurs) {
		super(javaName, javaType, parentBinding, minOccurs, maxOccurs);
		mComplexBinding = complexBinding;
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}

	/** {@inheritDoc} */
	public final int getByteLength() throws HostException {
		return getMaxOccurs() * mComplexBinding.getByteLength();
	}

	/**
	 * Returns the size of a single item within the array.
	 * @return single item byte length
	 * @throws HostException if size cannot be computed
	 */
	public final int getItemByteLength() throws HostException {
		return mComplexBinding.getByteLength();
	}

	/** {@inheritDoc} */
	public final ICobolBinding getItem(final int index)
		throws HostException {
		return mComplexBinding;
	}

	/**
	 * @return the Complex Binding describing an item
	 */
	public final CComplexBinding getComplexBinding() {
		return mComplexBinding;
	}

	/**
	 * @param complexBinding the item Complex Binding to set
	 */
	public final void setComplexBinding(
			final CComplexBinding complexBinding) {
		mComplexBinding = complexBinding;
	}

}
