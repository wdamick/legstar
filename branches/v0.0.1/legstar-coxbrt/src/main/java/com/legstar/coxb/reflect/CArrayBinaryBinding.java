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

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolArrayBinaryBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;


/**
 * This class implements the behavior of an array of binary cobol elements
 * bound to a JAXB numeric property.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayBinaryBinding
	extends CArrayNumericBinding
	implements ICobolArrayBinaryBinding {
	
	/**
	 * Constructor for an array of binary elements.
	 * 
	 * @param jaxbName the java property name of this array
	 * @param jaxbType the java property type of this array
	 * @param jaxbObject the concrete JAXB object instance bound to this array
	 * @param cobolAnnotations cobol annotations associated with this element
	 * @throws HostException if construction fails
	 */
	public CArrayBinaryBinding(
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
}