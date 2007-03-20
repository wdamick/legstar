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
package com.legstar.coxb.reflect;

import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;
import com.legstar.util.JaxbUtil;

/**
 * This class implements the behavior of an array of binary cobol elements
 * bound to a JAXB byte array.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayOctetStreamBinding
	extends CArraySimpleBinding
	implements ICobolArrayOctetStreamBinding {

	/**
	 * Constructor for arrays of byte array elements. The corresponding JAXB
	 * type is a variation on java.util.List < byte[] >.
	 * 
	 * @param jaxbName the java property name of this array
	 * @param jaxbType the java property type of this array
	 * @param jaxbObject the concrete JAXB object instance bound to this array
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CArrayOctetStreamBinding(
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
	@SuppressWarnings("unchecked")
	public final List < byte[] > getValue() throws HostException {
		Object result = JaxbUtil.invokeGetProperty(
				getJaxbObject(), getJavaName());
		if (result == null || ((List) result).size() == 0) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (getCobolAnnotations().isRedefined()
				|| 	getCobolAnnotations().redefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			ArrayList < byte[] > itemsList =
				new ArrayList < byte[] >(); 
			for (int i = 0; i < getMinOccurs(); i++) {
				itemsList.add(new byte[this.getItemByteLength()]);
			}
			return itemsList;
		}
		return (List < byte[] >) result;
	}

	/** {@inheritDoc} */
	@SuppressWarnings("unchecked")
	public final void setValue(final List < byte[] > iArray)
		throws HostException {
		List < byte[] > oArray = (List < byte[] >) 
		     JaxbUtil.invokeGetProperty(getJaxbObject(), getJavaName());
		oArray.clear();
		oArray.addAll(iArray);
	}

}
