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
package com.legstar.coxb.convert;

/**
 * 
 * This class holds references to a set of converters for a given strategy.
 *
 * @author Fady Moussallam
 * 
*/
public class CobolConverters {
	/** Cobol to Java String conversion strategy. */
	private ICobolStringConverter mCobolStringConverter;
	
	/** Cobol to Java Zoned decimal conversion strategy. */
	private ICobolZonedDecimalConverter mCobolZonedDecimalConverter;
	
	/** Cobol to Java Packed decimal conversion strategy. */
	private ICobolPackedDecimalConverter mCobolPackedDecimalConverter;
	
	/** Cobol to Java Binary conversion strategy. */
	private ICobolBinaryConverter mCobolBinaryConverter;
	
	/** Cobol to Java Float conversion strategy. */
	private ICobolFloatConverter mCobolFloatConverter;
	
	/** Cobol to Java Double conversion strategy. */
	private ICobolDoubleConverter mCobolDoubleConverter;
	
	/** Cobol to Java Octet stream conversion strategy. */
	private ICobolOctetStreamConverter mCobolOctetStreamConverter;

	/** Cobol to Java National conversion strategy. */
	private ICobolNationalConverter mCobolNationalConverter;

	/**
	 * @return Returns the cobolBinaryConverter.
	 */
	public final ICobolBinaryConverter getCobolBinaryConverter() {
		return mCobolBinaryConverter;
	}

	/**
	 * @param cobolBinaryConverter The cobolBinaryConverter to set.
	 */
	public final void setCobolBinaryConverter(
			final ICobolBinaryConverter cobolBinaryConverter) {
		mCobolBinaryConverter = cobolBinaryConverter;
	}

	/**
	 * @return Returns the cobolDoubleConverter.
	 */
	public final ICobolDoubleConverter getCobolDoubleConverter() {
		return mCobolDoubleConverter;
	}

	/**
	 * @param cobolDoubleConverter The cobolDoubleConverter to set.
	 */
	public final void setCobolDoubleConverter(
			final ICobolDoubleConverter cobolDoubleConverter) {
		mCobolDoubleConverter = cobolDoubleConverter;
	}

	/**
	 * @return Returns the cobolFloatConverter.
	 */
	public final ICobolFloatConverter getCobolFloatConverter() {
		return mCobolFloatConverter;
	}

	/**
	 * @param cobolFloatConverter The cobolFloatConverter to set.
	 */
	public final void setCobolFloatConverter(
			final ICobolFloatConverter cobolFloatConverter) {
		mCobolFloatConverter = cobolFloatConverter;
	}

	/**
	 * @return Returns the cobolOctetStreamConverter.
	 */
	public final ICobolOctetStreamConverter getCobolOctetStreamConverter() {
		return mCobolOctetStreamConverter;
	}

	/**
	 * @param cobolOctetStreamConverter The cobolOctetStreamConverter to set.
	 */
	public final void setCobolOctetStreamConverter(
			final ICobolOctetStreamConverter cobolOctetStreamConverter) {
		mCobolOctetStreamConverter = cobolOctetStreamConverter;
	}

	/**
	 * @return Returns the cobolPackedDecimalConverter.
	 */
	public final ICobolPackedDecimalConverter getCobolPackedDecimalConverter() {
		return mCobolPackedDecimalConverter;
	}

	/**
	 * @param cobolPackedDecimalConverter The cobolPackedDecimalConverter
	 * to set.
	 */
	public final void setCobolPackedDecimalConverter(
			final ICobolPackedDecimalConverter cobolPackedDecimalConverter) {
		mCobolPackedDecimalConverter = cobolPackedDecimalConverter;
	}

	/**
	 * @return Returns the cobolStringConverter.
	 */
	public final ICobolStringConverter getCobolStringConverter() {
		return mCobolStringConverter;
	}

	/**
	 * @param cobolStringConverter The cobolStringConverter to set.
	 */
	public final void setCobolStringConverter(
			final ICobolStringConverter cobolStringConverter) {
		mCobolStringConverter = cobolStringConverter;
	}

	/**
	 * @return Returns the cobolZonedDecimalConverter.
	 */
	public final ICobolZonedDecimalConverter getCobolZonedDecimalConverter() {
		return mCobolZonedDecimalConverter;
	}

	/**
	 * @param cobolZonedDecimalConverter The cobolZonedDecimalConverter to set.
	 */
	public final void setCobolZonedDecimalConverter(
			final ICobolZonedDecimalConverter cobolZonedDecimalConverter) {
		mCobolZonedDecimalConverter = cobolZonedDecimalConverter;
	}

	/**
	 * @return Returns the cobolNationalConverter.
	 */
	public final ICobolNationalConverter getCobolNationalConverter() {
		return mCobolNationalConverter;
	}

	/**
	 * @param cobolNationalConverter The cobolNationalConverter to set.
	 */
	public final void setCobolNationalConverter(
			final ICobolNationalConverter cobolNationalConverter) {
		mCobolNationalConverter = cobolNationalConverter;
	}

}
