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

import java.math.BigDecimal;

import com.legstar.coxb.ICobolBinding;
import com.legstar.host.HostException;

/**
 * This class is a superclass of all arrays of element types
 * implementing binding between a java type and cobol.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CArrayBinding
	extends CBinding implements ICobolBinding {

	/** When element bound belongs to a hierachy, this references the parent 
	 * binding. */
	private CComplexBinding mParentBinding;
	
	/**
	 * Constructor for a cobol array of elements to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param parentBinding a reference to the parent binding if any
	 * @param minOccurs minimum number of occurences
	 * @param maxOccurs maximum number of occurences
	 */
	public CArrayBinding(
			final String javaName,
			final String javaType,
			final CComplexBinding parentBinding,
			final int minOccurs,
			final int maxOccurs) {
		super(javaName, javaType);
		mParentBinding = parentBinding;
		setMinOccurs(minOccurs);
		setMaxOccurs(maxOccurs);
	}
	
	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		throw (new HostException("Element " + getJavaName()
				+ "cannot return a numeric value"));
	}


	/**
	 * @return the parent binding element
	 */
	public final CComplexBinding getParentBinding() {
		return mParentBinding;
	}

	/**
	 * @param parentBinding the arent binding element to set
	 */
	public final void setParentBinding(
			final CComplexBinding parentBinding) {
		mParentBinding = parentBinding;
	}

}
