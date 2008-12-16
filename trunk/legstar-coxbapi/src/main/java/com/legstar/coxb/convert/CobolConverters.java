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
package com.legstar.coxb.convert;

import com.legstar.coxb.CobolContext;

/**
 * 
 * This class holds references to a set of converters for a given strategy.
 * @deprecated
 *
 * @author Fady Moussallam
 * 
 */
public class CobolConverters implements ICobolConverters {
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
    
    /** Cobol compiler parameters. */
    private CobolContext mCobolContext;


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

    /**
     * @return Returns the CobolContext.
     */
    public final CobolContext getCobolContext() {
        return mCobolContext;
    }

    /**
     * @param cobolContext The CobolContext to set.
     */
    public final void setCobolContext(final CobolContext cobolContext) {
        mCobolContext = cobolContext;
    }

}
