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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.ICobolArrayNumericBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;
import com.legstar.util.JaxbUtil;


/**
 * This class is a super-class for the various types of Cobol numerics arrays.
 *
 * @author Fady Moussallam
 * 
 */
public abstract class CArrayNumericBinding
	extends CArraySimpleBinding
	implements ICobolArrayNumericBinding {
	
	/**
	 * Constructor for arrays of numeric elements. The corresponding JAXB type
	 * is a variation on java.util.List < E > where E is one of the numeric
	 * types supported by JAXB.
	 * @param jaxbName the java property name of this array
	 * @param jaxbType the java property type of this array
	 * @param jaxbObject the concrete JAXB object instance bound to this array
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CArrayNumericBinding(
			final String jaxbName,
			final String jaxbType,
			final Object jaxbObject,
			final CobolElement cobolAnnotations)
	throws HostException {
		
		super(jaxbName, jaxbType, jaxbObject, cobolAnnotations);
	}
	
	/**
	 * Returns the list of BigDecimal values from the associated array object.
	 * @return  numeric array items as a list of BigDecimals
	 * @throws HostException if list cannot be built
	 */
	public final List < BigDecimal > getValue() throws HostException {
		
		List < BigDecimal > lArray = new ArrayList < BigDecimal >();
		Object result = JaxbUtil.invokeGetProperty(
				getJaxbObject(), getJavaName());
		if (result == null || ((List) result).size() == 0) {
			/* If this element is involved in a redefinition, null means
			 * this alternative is not present so send back this information. */
			if (getCobolAnnotations().isRedefined()
				|| 	getCobolAnnotations().redefines().length() > 0) {
				return null;
			}
			/* Send back a default value */	
			ArrayList < BigDecimal > itemsList =
				new ArrayList < BigDecimal >(); 
			for (int i = 0; i < getMinOccurs(); i++) {
				itemsList.add(new BigDecimal("0"));
			}
			return itemsList;
		}
		
		List iArray = (List) result;
		switch (getJaxbArraySimpleType()) {
		case JAXB_ARRAY_BYTE_CLASS:
			for (Object obj : iArray) {
				lArray.add(new BigDecimal(((Byte) obj).toString()));
			}
			break;
		case JAXB_ARRAY_SHORT_CLASS:
			for (Object obj : iArray) {
				lArray.add(new BigDecimal(((Short) obj).toString()));
			}
			break;
		case JAXB_ARRAY_LONG_CLASS:
			for (Object obj : iArray) {
				lArray.add(new BigDecimal(((Long) obj).toString()));
			}
			break;
		case JAXB_ARRAY_INTEGER_CLASS:
			for (Object obj : iArray) {
				lArray.add(new BigDecimal(((Integer) obj).toString()));
			}
			break;
		case JAXB_ARRAY_FLOAT_CLASS:
			for (Object obj : iArray) {
				lArray.add(new BigDecimal(((Float) obj).toString()));
			}
			break;
		case JAXB_ARRAY_DOUBLE_CLASS:
			for (Object obj : iArray) {
				lArray.add(new BigDecimal(((Double) obj).toString()));
			}
			break;
		case JAXB_ARRAY_BIGINTEGER:
			for (Object obj : iArray) {
				lArray.add(new BigDecimal(((BigInteger) obj).toString()));
			}
			break;
		case JAXB_ARRAY_BIGDECIMAL:
			for (Object obj : iArray) {
				lArray.add((BigDecimal) obj);
			}
			break;
		default:
			throw (new HostException(
					"Cannot process object of class " + getType()));
		}
		return lArray;
	}
	
	/**
	 * Sets the list of BigDecimal values in the associated array object.
	 * @param  iArray list of BigDecimals
	 * @throws HostException if list cannot be built
	 */
	@SuppressWarnings("unchecked")
	public final void setValue(final List < BigDecimal > iArray)
		throws HostException {
		try {
			String getterName = "get"
				+ getJavaName().substring(0, 1).toUpperCase()
			+ getJavaName().substring(1);
			Method getter =
				getJaxbObject().getClass().getMethod(getterName);
			switch (getJaxbArraySimpleType()) {
			case JAXB_ARRAY_BYTE_CLASS:  
				List < Byte > oByte = 
					(List < Byte >) getter.invoke(getJaxbObject());
				oByte.clear();
				for (BigDecimal dec : iArray) {
					oByte.add(new Byte(dec.byteValue()));
				}
				break;
			case JAXB_ARRAY_SHORT_CLASS: 
				List < Short > oShort =
					(List < Short >) getter.invoke(getJaxbObject());
				oShort.clear();
				for (BigDecimal dec : iArray) {
					oShort.add(new Short(dec.shortValue()));
				}
				break;
			case JAXB_ARRAY_LONG_CLASS: 
				List < Long > oLong =
					(List < Long >) getter.invoke(getJaxbObject());
				oLong.clear();
				for (BigDecimal dec : iArray) {
					oLong.add(new Long(dec.longValue()));
				}
				break;
			case JAXB_ARRAY_INTEGER_CLASS:
				List < Integer > oInteger =
					(List < Integer >) getter.invoke(getJaxbObject());
				oInteger.clear();
				for (BigDecimal dec : iArray) {
					oInteger.add(new Integer(dec.intValue()));
				}
				break;
			case JAXB_ARRAY_FLOAT_CLASS:
				List < Float > oFloat =
					(List < Float >) getter.invoke(getJaxbObject());
				oFloat.clear();
				for (BigDecimal dec : iArray) {
					oFloat.add(new Float(dec.floatValue()));
				}
				break;
			case JAXB_ARRAY_DOUBLE_CLASS:
				List < Double > oDouble =
					(List < Double >) getter.invoke(getJaxbObject());
				oDouble.clear();
				for (BigDecimal dec : iArray) {
					oDouble.add(new Double(dec.doubleValue()));
				}
				break;
			case JAXB_ARRAY_BIGINTEGER:
				List < BigInteger > oBigInteger =
					(List < BigInteger >) getter.invoke(getJaxbObject());
				oBigInteger.clear();
				for (BigDecimal dec : iArray) {
					oBigInteger.add(dec.toBigInteger());
				}
				break;
			case JAXB_ARRAY_BIGDECIMAL:
				List < BigDecimal > oBigDecimal =
					(List < BigDecimal >) getter.invoke(getJaxbObject());
				oBigDecimal.clear();
				for (BigDecimal dec : iArray) {
					oBigDecimal.add(dec);
				}
				break;
			default:
				throw (new HostException(
						"Cannot process object of class " + getType()));
			}
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException " + e.getMessage()));
		} catch (NoSuchMethodException e) {
			throw (new HostException(
					"NoSuchMethodException " + e.getMessage()));
		} catch (InvocationTargetException e) {
			throw (new HostException(
					"InvocationTargetException " + e.getMessage()));
		}
	}
	
}
