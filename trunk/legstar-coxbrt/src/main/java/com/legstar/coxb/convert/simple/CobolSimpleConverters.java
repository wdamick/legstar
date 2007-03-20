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
package com.legstar.coxb.convert.simple;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.CobolConverters;

/**
 * This class holds references to a set of converters for the simple strategy.
 *
 * @author Fady Moussallam
 * 
*/
public class CobolSimpleConverters extends CobolConverters {
	
	/**
	 * Constructs a simple converters set.
	 * 
	 * @param cobolContext current Cobol compiler parameters
	 */
	public CobolSimpleConverters(final CobolContext cobolContext) {
		setCobolStringConverter(
				new CobolStringSimpleConverter(cobolContext));
		setCobolZonedDecimalConverter(
				new CobolZonedDecimalSimpleConverter(cobolContext));
		setCobolPackedDecimalConverter(
				new CobolPackedDecimalSimpleConverter(cobolContext));
		setCobolBinaryConverter(
				new CobolBinarySimpleConverter(cobolContext));
		setCobolFloatConverter(
				new CobolFloatSimpleConverter(cobolContext));
		setCobolDoubleConverter(
				new CobolDoubleSimpleConverter(cobolContext));
		setCobolOctetStreamConverter(
				new CobolOctetStreamSimpleConverter(cobolContext));
		setCobolNationalConverter(
				new CobolNationalSimpleConverter(cobolContext));
	}
}
