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
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.host.HostException;

/**
 * This class implements the behavior of a comp-1 cobol element bound to
 * a JAXB Float property.
 *
 * @author Fady Moussallam
 * 
*/
public class CFloatBinding
	extends CSingleSimpleBinding
	implements ICobolFloatBinding {
	
	/** The current value for this element. */
	private Float mValue = null;
	
	/** The java string type. */
	private static final String JAVA_BOUND_TYPE = "Float";
	
	/** The java string type. */
	private static final int COBOL_BYTE_LEN = 4;
	
	/**
	 * Constructor for a cobol float element to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 */
	public CFloatBinding(
			final String javaName) {
		
		super(javaName, JAVA_BOUND_TYPE, COBOL_BYTE_LEN);
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final Float getValue() throws HostException {
		
		if (mValue == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
				|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			mValue = 0f;
		}
		
		return mValue;
	}

	/** {@inheritDoc} */
	public final void setValue(final Float value) throws HostException {
		mValue = value;
	}

	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		return (new BigDecimal(mValue));
	}
	
}
