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
package com.legstar.coxb;

/**
 * This is an abstract representation of a binding factory used to shield
 * users from a particular binding implementation.
 */
public interface ICobolBindingFactory {
	
	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolStringBinding createStringBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolStringBinding createStringBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolOctetStreamBinding createOctetStreamBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolOctetStreamBinding createOctetStreamBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolNationalBinding createNationalBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolNationalBinding createNationalBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolZonedDecimalBinding createZonedDecimalBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolZonedDecimalBinding createZonedDecimalBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolPackedDecimalBinding createPackedDecimalBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolPackedDecimalBinding createPackedDecimalBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolBinaryBinding createBinaryBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolBinaryBinding createBinaryBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolFloatBinding createFloatBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolFloatBinding createFloatBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolDoubleBinding createDoubleBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolDoubleBinding createDoubleBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayStringBinding createArrayStringBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayStringBinding createArrayStringBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayOctetStreamBinding createArrayOctetStreamBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayOctetStreamBinding createArrayOctetStreamBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayNationalBinding createArrayNationalBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayNationalBinding createArrayNationalBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayZonedDecimalBinding createArrayZonedDecimalBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayZonedDecimalBinding createArrayZonedDecimalBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayPackedDecimalBinding createArrayPackedDecimalBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayPackedDecimalBinding createArrayPackedDecimalBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayBinaryBinding createArrayBinaryBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayBinaryBinding createArrayBinaryBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayFloatBinding createArrayFloatBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayFloatBinding createArrayFloatBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding without a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayDoubleBinding createArrayDoubleBinding(
			final String bindingName,
			final ICobolComplexBinding parentBinding);

	/**
	 * Create a cobol binding with a bound jaxb object.
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param parentBinding a reference to the parent binding (null if none)
	 * @return a concrete binding implementation
	 */
	ICobolArrayDoubleBinding createArrayDoubleBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final ICobolComplexBinding parentBinding);

}
