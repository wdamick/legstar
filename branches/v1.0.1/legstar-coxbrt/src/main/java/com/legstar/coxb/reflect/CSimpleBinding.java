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
 * This class is a superclass of all other JAXB simple element types. It
 * holds a reference to the cobol annotations for this element.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CSimpleBinding extends CBinding {

	/** This enum is used to speed up jaxb types dereferencing. */
	protected enum JaxbSimpleType {
		/** byte. */ 
		JAXB_BYTE,
		/** short. */ 
		JAXB_SHORT,
		/** long. */ 
		JAXB_LONG,
		/** int. */ 
		JAXB_INTEGER,
		/** float. */ 
		JAXB_FLOAT,
		/** double. */ 
		JAXB_DOUBLE,
		/** java.lang.String. */ 
		JAXB_STRING,
		/** byte[]. */ 
		JAXB_BYTE_ARRAY,
		/** java.lang.Byte. */ 
		JAXB_BYTE_CLASS,
		/** java.lang.Short. */ 
		JAXB_SHORT_CLASS,
		/** java.lang.Long. */ 
		JAXB_LONG_CLASS,
		/** java.lang.Integer. */ 
		JAXB_INTEGER_CLASS,
		/** java.lang.Float. */ 
		JAXB_FLOAT_CLASS,
		/** java.lang.Double. */ 
		JAXB_DOUBLE_CLASS,
		/** java.lang.BigInteger. */ 
		JAXB_BIGINTEGER,
		/** java.lang.BigDecimal. */ 
		JAXB_BIGDECIMAL 
	}

	/** Type of jaxb property. */
	private JaxbSimpleType mJaxbSimpleType;
	
	/** 
	 * Constructor for simple elements. The corresponding JAXB type
	 * is a simple java type such as int or Long.
	 * 
	 * @param jaxbName the name of the corresponding JAXB property
	 * @param jaxbType the java type of the corresponding JAXB property
	 * @param jaxbObject the concrete JAXB object instance bound to this element
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 * */
	public CSimpleBinding(
			final String jaxbName,
			final String jaxbType,
			final Object jaxbObject,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		super(jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		mJaxbSimpleType = getJaxbSimpleType(jaxbType);
	}

	/**
	 * Helper method to map jaxb string types to enum items.
	 * 
	 * @param jaxbType the String representing the jaxb type
	 * @return the corresponding enum value
	 * @throws HostException if this is an unknown jaxb type
	 */
	private JaxbSimpleType getJaxbSimpleType(final String jaxbType)
		throws HostException {
		
		if (jaxbType.compareTo("byte") == 0) {
			return JaxbSimpleType.JAXB_BYTE;
		}
		if (jaxbType.compareTo("short") == 0) {
			return JaxbSimpleType.JAXB_SHORT;
		}
		if (jaxbType.compareTo("long") == 0) {
			return JaxbSimpleType.JAXB_LONG;
		}
		if (jaxbType.compareTo("int") == 0) {
			return JaxbSimpleType.JAXB_INTEGER;
		}
		if (jaxbType.compareTo("float") == 0) {
			return JaxbSimpleType.JAXB_FLOAT;
		}
		if (jaxbType.compareTo("double") == 0) {
			return JaxbSimpleType.JAXB_DOUBLE;
		}
		if (jaxbType.compareTo("java.lang.String") == 0) {
			return JaxbSimpleType.JAXB_STRING;
		}
		if (jaxbType.compareTo("byte[]") == 0) {
			return JaxbSimpleType.JAXB_BYTE_ARRAY;
		}
		if (jaxbType.compareTo("[B") == 0) {
			return JaxbSimpleType.JAXB_BYTE_ARRAY;
		}
		if (jaxbType.compareTo("java.lang.Byte") == 0) {
			return JaxbSimpleType.JAXB_BYTE_CLASS;
		}
		if (jaxbType.compareTo("java.lang.Short") == 0) {
			return JaxbSimpleType.JAXB_SHORT_CLASS;
		}
		if (jaxbType.compareTo("java.lang.Long") == 0) {
			return JaxbSimpleType.JAXB_LONG_CLASS;
		}
		if (jaxbType.compareTo("java.lang.Integer") == 0) {
			return JaxbSimpleType.JAXB_INTEGER_CLASS;
		}
		if (jaxbType.compareTo("java.lang.Float") == 0) {
			return JaxbSimpleType.JAXB_FLOAT_CLASS;
		}
		if (jaxbType.compareTo("java.lang.Double") == 0) {
			return JaxbSimpleType.JAXB_DOUBLE_CLASS;
		}
		if (jaxbType.compareTo("java.math.BigInteger") == 0) {
			return JaxbSimpleType.JAXB_BIGINTEGER;
		}
		if (jaxbType.compareTo("java.math.BigDecimal") == 0) {
			return JaxbSimpleType.JAXB_BIGDECIMAL;
		} else {
			throw (new HostException(
					"Cannot process object of class " + jaxbType));
		}
	}

	/**
	 * @return Returns the Jaxb enum type.
	 */
	public final JaxbSimpleType getJaxbSimpleType() {
		return mJaxbSimpleType;
	}

	/** {@inheritDoc} */
	public final int getByteLength() throws HostException {
		return getCobolAnnotations().byteLength();
	}

	
}
