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
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.host.HostException;

import java.util.ArrayList;
import java.util.List;

/**
 * This class implements the behavior of an array of national cobol elements
 * bound to a JAXB String property.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayNationalBinding
	extends CArraySimpleBinding
	implements ICobolArrayNationalBinding {
	
	/** The current list for this array. */
	private List < String > mList = null;
	
	/** The java string type. */
	private static final String JAVA_BOUND_TYPE =
		"List < java.lang.String >";
	
	/**
	 * Constructor for a cobol array of national elements to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param parentBinding a reference to the parent binding if any
	 * @param itemByteLength length in bytes of a single cobol item
	 * @param isJustifiedRight true if cobol string is right justified
	 * @param minOccurs minimum number of occurences
	 * @param maxOccurs maximum number of occurences
	 */
	public CArrayNationalBinding(
			final String javaName,
			final CComplexBinding parentBinding,
			final int itemByteLength,
			final boolean isJustifiedRight,
			final int minOccurs,
			final int maxOccurs) {
		
		super(javaName, JAVA_BOUND_TYPE, parentBinding,
				itemByteLength, minOccurs, maxOccurs);
		setIsJustifiedRight(isJustifiedRight);
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final List < String > getValue() throws HostException {
		
		if (mList == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
				|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			mList = new ArrayList < String >();
		}
		
		return mList;
	}

	/** {@inheritDoc} */
	public final void setValue(
			final List < String > list) throws HostException {
		mList = list;
	}
	/**
	 * @return the List of items
	 */
	public final List < String > getList() {
		return mList;
	}

	/**
	 * @param list the items List to set
	 */
	public final void setList(
			final List < String > list) {
		mList = list;
	}


}
