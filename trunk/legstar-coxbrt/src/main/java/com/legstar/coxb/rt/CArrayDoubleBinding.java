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
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.host.HostException;

import java.util.ArrayList;
import java.util.List;

/**
 * This class implements the behavior of an array of comp-2 cobol elements
 * bound to a JAXB Double property.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayDoubleBinding
	extends CArraySimpleBinding
	implements ICobolArrayDoubleBinding {
	
	/** The current list for this array. */
	private List < Double > mList = null;
	
	/** The java string type. */
	private static final String JAVA_BOUND_TYPE =
		"List < Double >";
	
	/** The java string type. */
	private static final int COBOL_BYTE_LEN = 8;
	
	/**
	 * Constructor for a cobol array comp-2 element to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param parentBinding a reference to the parent binding if any
	 * @param minOccurs minimum number of occurences
	 * @param maxOccurs maximum number of occurences
	 */
	public CArrayDoubleBinding(
			final String javaName,
			final CComplexBinding parentBinding,
			final int minOccurs,
			final int maxOccurs) {
		
		super(javaName, JAVA_BOUND_TYPE, parentBinding,
				COBOL_BYTE_LEN, minOccurs, maxOccurs);
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final List < Double > getValue() throws HostException {
		
		if (mList == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
				|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			mList = new ArrayList < Double >();
		}
		
		return mList;
	}

	/** {@inheritDoc} */
	public final void setValue(
			final List < Double > list) throws HostException {
		mList = list;
	}
	/**
	 * @return the List of items
	 */
	public final List < Double > getList() {
		return mList;
	}

	/**
	 * @param list the items List to set
	 */
	public final void setList(
			final List < Double > list) {
		mList = list;
	}

}
