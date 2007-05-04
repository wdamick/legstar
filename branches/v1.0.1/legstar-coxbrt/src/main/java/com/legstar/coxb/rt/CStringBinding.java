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
package com.legstar.coxb.rt;

import java.math.BigDecimal;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.host.HostException;

/**
 * This class implements the behavior of a string cobol element bound to
 * a JAXB String property.
 *
 * @author Fady Moussallam
 * 
*/
public class CStringBinding
	extends CSingleSimpleBinding
	implements ICobolStringBinding {
	
	/** The current value for this element. */
	private String mValue = null;
	
	/** The java string type. */
	private static final String JAVA_BOUND_TYPE = "java.lang.String";
	
	/**
	 * Constructor for a cobol string element to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param byteLength the cobol element length in bytes
	 * @param isJustifiedRight true if cobol string is right justified
	 */
	public CStringBinding(
			final String javaName,
			final int byteLength,
			final boolean isJustifiedRight) {
		
		super(javaName, JAVA_BOUND_TYPE, byteLength);
		setIsJustifiedRight(isJustifiedRight);
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final String getValue() throws HostException {
		
		if (mValue == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
				|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			mValue = "";
		}
		
		
		return mValue;
	}

	/** {@inheritDoc} */
	public final void setValue(final String value) throws HostException {
		mValue = value;
	}

	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		throw (new HostException("Element " + getJavaName()
				+ "cannot return a numeric value"));
	}
	
}
