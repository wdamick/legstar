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
package com.legstar.coxb.reflect;

import java.math.BigDecimal;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;
import com.legstar.util.JaxbUtil;

/**
 * This class implements the behavior of a national cobol element bound to
 * a JAXB String property.
 *
 * @author Fady Moussallam
 * 
*/
public class CNationalBinding
	extends CSimpleBinding
	implements ICobolNationalBinding {
	
	/**
	 * Constructor for national elements. The corresponding JAXB type is String.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param jaxbObject the concrete JAXB object instance bound to this element
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CNationalBinding(
			final String jaxbName,
			final String jaxbType,
			final Object jaxbObject,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		super(jaxbName, jaxbType, jaxbObject, cobolAnnotations);
	}

	/** {@inheritDoc} */
	@Override
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final String getValue() throws HostException {
		
		String value;
		Object result = JaxbUtil.invokeGetProperty(
				getJaxbObject(), getJavaName());
		if (result == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
				|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			value = "";
		} else {
			value = (String) result;
		}
		
		return value;
	}

	/** {@inheritDoc} */
	public final void setValue(final String value) throws HostException {
		JaxbUtil.invokeSetProperty(
				getJaxbObject(), getJavaName(), value, String.class);
	}

	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		throw (new HostException("Element " + getJavaName()
				+ "cannot return a numeric value"));
	}

}
