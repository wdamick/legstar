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
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.host.HostException;

import java.util.ArrayList;
import java.util.List;

/**
 * This class implements the behavior of an array of binary cobol elements
 * bound to a JAXB byte array property.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayOctetStreamBinding extends CBinding
	implements ICobolArrayOctetStreamBinding {
	
	/** The current list for this array. */
	private List < byte[] > mList = null;
	
	/**
	 * Creates an empty binding between a Cobol array of byte elements 
	 * and a java List of byte arrays.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 */
	public CArrayOctetStreamBinding(
			final String javaName,
			final Class javaType) {
		
		super(javaName, javaType);
	}
	
	/**
	 * Creates a binding between a Cobol array of byte elements and a
	 * java List of byte arrays.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 */
	public CArrayOctetStreamBinding(
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

	/** {@inheritDoc} */
	public final List < byte[] > getValue() throws HostException {
		
		if (mList == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
				|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			mList = new ArrayList < byte[] >();
		}
		
		return mList;
	}

	/** {@inheritDoc} */
	public final void setValue(
			final List < byte[] > list) throws HostException {
		mList = list;
	}

	/** {@inheritDoc} */
	@SuppressWarnings("unchecked")
	public final void setValue(final Object value) throws HostException {
		if (value == null) {
			mList = null;
			return;
		}
		if (value instanceof List) {
			mList = (List < byte[] >) value;
		} else {
			throw new HostException(
					"Value passed to " + getJavaName() + " is not a List");
		}
	}
	
	/**
	 * @return the List of items
	 */
	public final List < byte[] > getByteArrayList() {
		return mList;
	}

	/**
	 * @param list the items List to set
	 */
	public final void setByteArrayList(
			final List < byte[] > list) {
		mList = list;
	}

	/** {@inheritDoc} */
	public final Object getObjectValue(final Class type) throws HostException {
		if (type.equals(byte[].class)) {
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
				mList = new ArrayList < byte[] >();
				return;
			}
			/* We assume all items will have the same type as the first one.
			 * The unchecked cast might break at runtime. */
			Object item = ((List) value).get(0);
			if (item instanceof byte[]) {
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
