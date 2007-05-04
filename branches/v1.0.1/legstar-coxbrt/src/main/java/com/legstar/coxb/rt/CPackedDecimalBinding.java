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

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.host.HostException;

import java.math.BigDecimal;

/**
 * This class implements the behavior of a packed decimal cobol element bound to
 * a JAXB BigDecimal property.
 *
 * @author Fady Moussallam
 * 
*/
public class CPackedDecimalBinding
	extends CSingleSimpleBinding
	implements ICobolPackedDecimalBinding {
	
	/** The current value for this element. */
	private BigDecimal mValue = null;
	
	/** The java string type. */
	private static final String JAVA_BOUND_TYPE = "java.math.BigDecimal";

	/**
	 * Constructor for a cobol packed decimal element to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param byteLength the cobol element length in bytes
	 * @param totalDigits cobol numeric total number of digits
	 * @param fractionDigits cobol numeric fractional number of digits
	 * @param isSigned true if cobol numeric is signed
	 */
	public CPackedDecimalBinding(
			final String javaName,
			final int byteLength,
			final int totalDigits,
			final int fractionDigits,
			final boolean isSigned) {
		
		super(javaName, JAVA_BOUND_TYPE, byteLength);
		setTotalDigits(totalDigits);
		setFractionDigits(fractionDigits);
		setIsSigned(isSigned);
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final BigDecimal getValue() throws HostException {
		
		if (mValue == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
				|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			mValue = new BigDecimal("0");
		}
		
		return mValue;
	}

	/** {@inheritDoc} */
	public final void setValue(final BigDecimal value) throws HostException {
		mValue = value;
	}

	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		return mValue;
	}
	
}
