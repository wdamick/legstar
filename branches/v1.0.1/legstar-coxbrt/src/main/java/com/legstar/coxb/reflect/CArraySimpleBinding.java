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

import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;

/**
 * Super class for Cobol/JAXB implementation of arrays of simple types.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CArraySimpleBinding
	extends CArrayBinding {

	/** This enum is used to speed up jaxb array types dereferencing. */
	protected enum JaxbArraySimpleType {
		/** java.util.List < byte[] >. */
		JAXB_ARRAY_BYTE_ARRAY,
		/** java.util.List < java.lang.String >. */
		JAXB_ARRAY_STRING,
		/** java.util.List < java.lang.Byte >. */ 
		JAXB_ARRAY_BYTE_CLASS,
		/** java.util.List < java.lang.Short >. */ 
		JAXB_ARRAY_SHORT_CLASS,
		/** java.util.List < java.lang.Long >. */ 
		JAXB_ARRAY_LONG_CLASS,
		/** java.util.List < java.lang.Integer >. */ 
		JAXB_ARRAY_INTEGER_CLASS,
		/** java.util.List < java.lang.Float >. */ 
		JAXB_ARRAY_FLOAT_CLASS,
		/** java.util.List < java.lang.Double >. */ 
		JAXB_ARRAY_DOUBLE_CLASS,
		/** java.util.List < java.lang.BigInteger >. */ 
		JAXB_ARRAY_BIGINTEGER,
		/** java.util.List < java.lang.BigDecimal > . */ 
		JAXB_ARRAY_BIGDECIMAL 
	}
	
	/** Type of jaxb array. */
	private JaxbArraySimpleType mJaxbArraySimpleType;

	/**
	 * Constructor for arrays of simple elements. The corresponding JAXB type
	 * is a variation on java.util.List < E > .
	 * 
	 * @param jaxbName the java property name of this array
	 * @param jaxbType the java property type of this array
	 * @param jaxbObject the concrete JAXB object instance bound to this array
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CArraySimpleBinding(
			final String jaxbName,
			final String jaxbType,
			final Object jaxbObject,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		super(jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		mJaxbArraySimpleType = getJaxbArraySimpleType(jaxbType);
	}
	
	
	/** {@inheritDoc} */
	@Override
	public final int getByteLength() throws HostException {
		return getMaxOccurs() * getItemByteLength();
	}
	
	/**
	 * Returns the size of a single item within the array.
	 * @return single item byte length
	 * @throws HostException if size cannot be computed
	 */
	public final int getItemByteLength() throws HostException {
		return getCobolAnnotations().byteLength();
	}
	
	/**
	 * Helper method to map jaxb string types to enum items.
	 * 
	 * @param jaxbType the String representing the jaxb type
	 * @return the corresponding enum value
	 * @throws HostException if this is an unknown jaxb type
	 */
	private JaxbArraySimpleType getJaxbArraySimpleType(final String jaxbType)
		throws HostException {
		
		if (jaxbType.compareTo("java.util.List<byte[]>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_BYTE_ARRAY;
		}
		if (jaxbType.compareTo("java.util.List<java.lang.String>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_STRING;
		}
		if (jaxbType.compareTo("java.util.List<java.lang.Byte>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_BYTE_CLASS;
		}
		if (jaxbType.compareTo("java.util.List<java.lang.Short>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_SHORT_CLASS;
		}
		if (jaxbType.compareTo("java.util.List<java.lang.Long>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_LONG_CLASS;
		}
		if (jaxbType.compareTo("java.util.List<java.lang.Integer>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_INTEGER_CLASS;
		}
		if (jaxbType.compareTo("java.util.List<java.lang.Float>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_FLOAT_CLASS;
		}
		if (jaxbType.compareTo("java.util.List<java.lang.Double>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_DOUBLE_CLASS;
		}
		if (jaxbType.compareTo("java.util.List<java.math.BigInteger>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_BIGINTEGER;
		}
		if (jaxbType.compareTo("java.util.List<java.math.BigDecimal>") == 0) {
			return JaxbArraySimpleType.JAXB_ARRAY_BIGDECIMAL;
		} else {
			throw (new HostException(
					"Cannot process object of class " + jaxbType));
		}
	}

	/**
	 * @return Returns the Jaxb enum type.
	 */
	public final JaxbArraySimpleType getJaxbArraySimpleType() {
		return mJaxbArraySimpleType;
	}


}
