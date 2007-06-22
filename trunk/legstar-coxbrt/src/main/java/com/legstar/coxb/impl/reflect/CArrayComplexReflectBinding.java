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
package com.legstar.coxb.impl.reflect;

import java.util.ArrayList;
import java.util.List;

import com.legstar.binding.CobolElement;
import com.legstar.host.HostException;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.impl.CArrayComplexBinding;

/**
 * Cobol/JAXB implementation of an array of complex (record) elements.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayComplexReflectBinding extends CArrayComplexBinding {

	/** This is a reference to a JAXB object factory. */
	private Object mJaxbObjectFactory;

    /** Java object to which this cobol complex array element is bound. */
    private List mJaxbObject;
  
	/**
	 * Creates a binding between a Cobol array of complex elements and a
	 * java List.
	 * 
	 * @param objectFactory the JAXB object factory
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param complexItemBinding a binding element for array items
	 */
	public CArrayComplexReflectBinding(
			final Object objectFactory,
			final String javaName,
			final Class javaType,
			final CobolElement cobolAnnotations,
			final ICobolComplexBinding complexItemBinding) {
		
		super(javaName, javaType, cobolAnnotations, complexItemBinding);
		mJaxbObjectFactory = objectFactory;
	}

	/** {@inheritDoc} */
	public final void createJaxbObject() throws HostException {
		mJaxbObject = new ArrayList();
	}
	
	/** {@inheritDoc} */
	public final void setItemValue(
			final int index) throws HostException {
        /* Make sure there is an associated JAXB object*/
    	if (mJaxbObject == null) {
    		createJaxbObject();
    	}
		getComplexItemBinding().setObjectValue(mJaxbObject.get(index));
	}
	
	/** {@inheritDoc} */
	@SuppressWarnings("unchecked")
	public final void addJaxbPropertyValue(
			final int index) throws HostException {
        /* Make sure there is an associated JAXB object*/
    	if (mJaxbObject == null) {
    		throw new HostException(
    				"Binded object not initialized for " + getJavaName());
    	}
		mJaxbObject.add(getComplexItemBinding().getObjectValue(getJavaType()));
	}

	/**
	 * @return Returns the JAXB Object Factory.
	 */
	public final Object getObjectFactory() {
		return mJaxbObjectFactory;
	}

    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
    	if (type.equals(getJavaType())) {
    		return mJaxbObject;
		} else {
			throw new HostException("Attempt to get binding " + getJavaName()
					+ " as an incompatible type " + type);
    	}
    }

    /** {@inheritDoc} */
    public final void setObjectValue(final Object value) throws HostException {
    	if (value == null) {
    		mJaxbObject = null;
    		return;
    	}
		if (value instanceof List) {
			if (((List) value).size() == 0) {
				mJaxbObject = new ArrayList();
				return;
			}
			/* We assume all items will have the same type as the first one.
			 * The unchecked cast might break at runtime. */
			Object item = ((List) value).get(0);
			if (item.getClass().equals(getJavaType())) {
				mJaxbObject = (List) value;
				return;
			}
		}
		throw new HostException("Attempt to set binding " + getJavaName()
				+ " from an incompatible value " + value);
    }

    /** {@inheritDoc} */
	public final boolean isSet() {
		return (mJaxbObject != null);
	}
}
