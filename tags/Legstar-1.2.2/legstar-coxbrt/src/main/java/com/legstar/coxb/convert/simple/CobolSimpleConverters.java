/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
