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

import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.coxb.ICobolNumericBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;
import com.legstar.util.JaxbUtil;


/**
 * This class is a super-class for the various types of Cobol single numerics.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CNumericBinding
	extends CSimpleBinding
	implements ICobolNumericBinding {

	/**
	 * Constructor for numeric elements. The corresponding JAXB type is any
	 * form of simple numeric (int, long, ...).
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param jaxbObject the concrete JAXB object instance bound to this element
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CNumericBinding(
			final String jaxbName,
			final String jaxbType,
			final Object jaxbObject,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		super(jaxbName, jaxbType, jaxbObject, cobolAnnotations);
	}

	/** {@inheritDoc} */
	public final BigDecimal getValue() throws HostException {
		
		BigDecimal value = null;
		Object result = JaxbUtil.invokeGetProperty(
				getJaxbObject(), getJavaName());
		if (result == null) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (isRedefined()
					|| 	getRedefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			value = new BigDecimal("0");
		} else {
			switch (getJaxbSimpleType()) {
			case JAXB_BYTE_CLASS:
			case JAXB_BYTE:
				value = new BigDecimal((Byte) result);
				break;
			case JAXB_SHORT_CLASS:
			case JAXB_SHORT:
				value = new BigDecimal((Short) result);
				break;
			case JAXB_LONG_CLASS:
			case JAXB_LONG:
				value = new BigDecimal((Long) result);
				break;
			case JAXB_INTEGER_CLASS:
			case JAXB_INTEGER:
				value = new BigDecimal((Integer) result);
				break;
			case JAXB_FLOAT_CLASS:
			case JAXB_FLOAT:
				value = new BigDecimal((Float) result);
				break;
			case JAXB_DOUBLE_CLASS:
			case JAXB_DOUBLE:
				value = new BigDecimal((Double) result);
				break;
			case JAXB_BIGINTEGER:
				value = new BigDecimal((BigInteger) result);
				break;
			case JAXB_BIGDECIMAL:
				value = (BigDecimal) result;
				break;
			default:
				throw (new HostException(
						"Cannot process object of class " + getType()));
			}
		}
		
		return value;
	}

	/** {@inheritDoc} */
	public final void setValue(final BigDecimal value) throws HostException {
		
		switch (getJaxbSimpleType()) {
		case JAXB_BYTE_CLASS:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), new Byte(value.byteValue()),
					Byte.class);
			break;
		case JAXB_BYTE:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), value.byteValue(),
					byte.class);
			break;
		case JAXB_SHORT_CLASS:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(),
					new Short(value.shortValue()),
					Short.class);
			break;
		case JAXB_SHORT:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), value.shortValue(),
					short.class);
			break;
		case JAXB_LONG_CLASS:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), new Long(value.longValue()),
					Long.class);
			break;
		case JAXB_LONG:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), value.longValue(),
					long.class);
			break;
		case JAXB_INTEGER_CLASS:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(),
					new Integer(value.intValue()),
					Integer.class);
			break;
		case JAXB_INTEGER:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), value.intValue(),
					int.class);
			break;
		case JAXB_FLOAT_CLASS:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(),
					new Float(value.floatValue()),
					Float.class);
			break;
		case JAXB_FLOAT:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), value.floatValue(),
					float.class);
			break;
		case JAXB_DOUBLE_CLASS:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(),
					new Double(value.doubleValue()),
					Double.class);
			break;
		case JAXB_DOUBLE:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), value.doubleValue(),
					double.class);
			break;
		case JAXB_BIGINTEGER:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), value.toBigInteger(),
					BigInteger.class);
			break;
		case JAXB_BIGDECIMAL:
			JaxbUtil.invokeSetProperty(
					getJaxbObject(), getJavaName(), value, BigDecimal.class);
			break;
		default:
			throw (new HostException(
					"Cannot process object of class " + getType()));
		}
	}
	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		return getValue();
	}

}
