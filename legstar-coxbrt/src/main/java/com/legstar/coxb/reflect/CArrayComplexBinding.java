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
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;
import com.legstar.util.JaxbUtil;

/**
 * Cobol/JAXB implementation of an array of complex (record) elements. This
 * class holds a reference to a JAXB object and implements an 'accept' method
 * conformant to the visitor pattern.
 *
 * @author Fady Moussallam
 * 
*/
public class CArrayComplexBinding
		extends CArrayBinding
		implements ICobolArrayComplexBinding {

	/** This is a reference to a JAXB object factory. */
	private Object mJaxbObjectFactory;

	/** Reference to the complex binding representing each occurence of this 
	 * array. */
	private CComplexBinding mComplexBinding;
	
	/**
	 * Constructor for arrays of complex elements. The corresponding JAXB type
	 * is a variation on java.util.List < E > where E is a complex type.
	 * 
	 * @param objectFactory the JAXB object factory
	 * @param jaxbName the java property name of this array
	 * @param jaxbType the java property type of this array
	 * @param jaxbObject the concrete JAXB object instance bound to this array
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CArrayComplexBinding(
			final Object objectFactory,
			final String jaxbName,
			final String jaxbType,
			final Object jaxbObject,
			final CobolElement cobolAnnotations)
		throws HostException {
		super(jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		mJaxbObjectFactory = objectFactory;
	}

	/** {@inheritDoc} */
	public final void createBoundObject() throws HostException {
	}
	
	/** {@inheritDoc} */
	public final void prepareChildren() throws HostException {
	}
	
	/** {@inheritDoc} */
	public final void initBoundItem(
			final int index) throws HostException {
		
		/* Create a new JAXB item */
		Object item = JaxbUtil.addComplexIndexedProperty(
				getObjectFactory(), getJaxbObject(), getType(), index);
		
		/* Create a binding for this item. We don't specify a complex parent
		 * item since there isn't any */
		mComplexBinding = new CComplexBinding(getJavaName(), null,
				getObjectFactory(),	item, getCobolAnnotations());
	}
	
	/** {@inheritDoc} */
	public final void getValuesFromBoundItem(
			final int index) throws HostException {
		
		/* Try to get item requested from the jaxb object */
		Object item = JaxbUtil.invokeGetIndexedProperty(getJaxbObject(), index);
		
		/* If item does not exist yet, create one */
		if (item == null) {
			item = JaxbUtil.addComplexIndexedProperty(
					getObjectFactory(), getJaxbObject(), getType(), index);
		}
		
		/* Create a binding for this item. We don't specify a complex parent
		 * item since there isn't any */
		mComplexBinding = new CComplexBinding(getJavaName(), null,
				getObjectFactory(),	item, getCobolAnnotations());
	}
	
	/** {@inheritDoc} */
	@Override
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final void setBoundItemValues(
			final int index) throws HostException {
		/* Bound items are already set by the accept process */
	}

	/** {@inheritDoc} */
	@Override
	public final int getByteLength() throws HostException {
		
		/* There were 2 possibilities here, either asking each item to give
		 * its length back which is time consuming but let items have differing
		 * length or, assume all items have same length and therefore only
		 * ask the first item to send its length back. Since in Cobol, items
		 * cannot normally be of varying sizes, we take the second option. */
		getValuesFromBoundItem(0);
		ICobolBinding itemDesc = getItem(0);
		int byteLength = itemDesc.getByteLength() * getMaxOccurs();
		return byteLength;
	}

	/** {@inheritDoc} */
	public final ICobolBinding getItem(final int index)
		throws HostException {
		return mComplexBinding;
	}
	
	/**
	 * @return Returns the JAXB Object Factory.
	 */
	public final Object getObjectFactory() {
		return mJaxbObjectFactory;
	}

	/** {@inheritDoc} */
	public final Object getValue() throws HostException {
		return getJaxbObject();
	}

	/**
	 * @return the Complex Binding describing an item
	 */
	public final CComplexBinding getComplexBinding() {
		return mComplexBinding;
	}

	/**
	 * @param complexBinding the Complex Binding describing an item to set
	 */
	public final void setComplexBinding(
			final CComplexBinding complexBinding) {
		mComplexBinding = complexBinding;
	}


}
