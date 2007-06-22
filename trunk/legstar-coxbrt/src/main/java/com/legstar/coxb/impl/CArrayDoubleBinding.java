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
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.host.HostException;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * This class implements the behavior of an array of comp-2 cobol elements
 * bound to a JAXB Double property.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayDoubleBinding extends CBinding
	implements ICobolArrayDoubleBinding {
	
	/** The current list for this array. */
	private List < Double > mList = null;
	
	/**
	 * Creates an empty binding between a Cobol array of double elements
	 * and a java List of Doubles.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 */
	public CArrayDoubleBinding(
			final String javaName,
			final Class javaType) {
		
		super(javaName, javaType);
	}
	
	/**
	 * Creates a binding between a Cobol array of double elements and a
	 * java List of Doubles.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 */
	public CArrayDoubleBinding(
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
	public final List < Double > getDoubleList() {
		return mList;
	}

	/**
	 * @param list the items List to set
	 */
	public final void setDoubleList(
			final List < Double > list) {
		mList = list;
	}

	/**
	 * @return the internal List as BigDecimals
	 */
	public final List < BigDecimal > getBigDecimalList() {
		List < BigDecimal > list = new ArrayList < BigDecimal >();
		for (Double value : mList) {
			list.add(new BigDecimal(value));
		}
		return list;
	}

	/**
	 * @param list the internal List of BigDecimals to set
	 */
	public final void setBigDecimalList(
			final List < BigDecimal > list) {
		mList = new ArrayList < Double >();
		for (BigDecimal value : list) {
			mList.add(value.doubleValue());
		}
	}

	/** {@inheritDoc} */
	public final Object getObjectValue(final Class type) throws HostException {
		if (type.equals(Double.class)) {
			return mList;
		} else if (type.equals(BigDecimal.class)) {
			return getBigDecimalList();
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
				mList = new ArrayList < Double >();
				return;
			}
			/* We assume all items will have the same type as the first one.
			 * The unchecked cast might break at runtime. */
			Object item = ((List) value).get(0);
			if (item instanceof Double) {
				mList = (List) value;
				return;
			} else if (item instanceof BigDecimal) {
				setBigDecimalList((List) value);
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
