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

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.host.HostException;

/**
 * This class implements the behavior of a binary cobol element bound to
 * a JAXB byte array property.
 *
 * @author Fady Moussallam
 * 
*/
public class COctetStreamBinding
	extends CSingleSimpleBinding
	implements ICobolOctetStreamBinding {
	
	/** The current value for this element. */
	private byte[] mValue = null;
	
	/** The java string type. */
	private static final String JAVA_BOUND_TYPE = "byte[]";
	
	/**
	 * Constructor for a cobol binary element to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param byteLength the cobol element length in bytes
	 */
	public COctetStreamBinding(
			final String javaName,
			final int byteLength) {
		
		super(javaName, JAVA_BOUND_TYPE, byteLength);
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final byte[] getValue() throws HostException {
		
		if (mValue == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
				|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			mValue = new byte[getByteLength()];
		}
		
		
		return mValue;
	}

	/** {@inheritDoc} */
	public final void setValue(final byte[] value) throws HostException {
		mValue = value;
	}

	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		throw (new HostException("Element " + getJavaName()
				+ "cannot return a numeric value"));
	}
	
}
