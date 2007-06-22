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
package com.legstar.coxb.impl;

import com.legstar.binding.CobolElement;
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
public class CArrayNationalBinding	extends CBinding
	implements ICobolArrayNationalBinding {
	
	/** The current list for this array. */
	private List < String > mList = null;
	
	/**
	 * Creates an empty binding between a Cobol array of national elements
	 * and a java List of Strings.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 */
	public CArrayNationalBinding(
			final String javaName,
			final Class javaType) {
		
		super(javaName, javaType);
	}
	
	/**
	 * Creates a binding between a Cobol array of national elements and a
	 * java List of Strings.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 */
	public CArrayNationalBinding(
			final String javaName,
			final Class javaType,
			final CobolElement cobolAnnotations) {
		
		super(javaName, javaType, cobolAnnotations);
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final int calcByteLength() throws HostException {
		return getMaxOccurs() * getByteLength();
	}

	/**
	 * @return the List of items
	 */
	public final List < String > getStringList() {
		return mList;
	}

	/**
	 * @param list the items List to set
	 */
	public final void setStringList(
			final List < String > list) {
		mList = list;
	}

	/** {@inheritDoc} */
	public final Object getObjectValue(final Class type) throws HostException {
		if (type.equals(String.class)) {
			return mList;
		} else {
			throw new HostException("Attempt to get binding " + getJavaName()
					+ " as an incompatible type " + type);
		}
	}

	/** {@inheritDoc} */
	@SuppressWarnings("unchecked")
	public final void setObjectValue(final Object value) throws HostException {
		if (value == null) {
			mList = null;
			return;
		}
		if (value instanceof List) {
			if (((List) value).size() == 0) {
				mList = new ArrayList < String >();
				return;
			}
			/* We assume all items will have the same type as the first one.
			 * The unchecked cast might break at runtime. */
			Object item = ((List) value).get(0);
			if (item instanceof String) {
				mList = (List) value;
				return;
			}
		}
		throw new HostException("Attempt to set binding " + getJavaName()
				+ " from an incompatible value " + value);
	}

	/** {@inheritDoc} */
	public final boolean isSet() {
		return (mList != null);
	}
}
