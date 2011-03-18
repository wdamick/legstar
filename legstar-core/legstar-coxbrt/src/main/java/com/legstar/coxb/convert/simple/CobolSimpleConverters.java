/*******************************************************************************
 * Copyright (c) 2011 LegSem.
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
import com.legstar.coxb.convert.ICobolBinaryConverter;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.ICobolDbcsConverter;
import com.legstar.coxb.convert.ICobolDoubleConverter;
import com.legstar.coxb.convert.ICobolFloatConverter;
import com.legstar.coxb.convert.ICobolNationalConverter;
import com.legstar.coxb.convert.ICobolOctetStreamConverter;
import com.legstar.coxb.convert.ICobolPackedDecimalConverter;
import com.legstar.coxb.convert.ICobolStringConverter;
import com.legstar.coxb.convert.ICobolZonedDecimalConverter;

/**
 * This class holds references to a set of converters for the simple strategy.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolSimpleConverters implements ICobolConverters {

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
    
    /** Cobol to Java DBCS conversion strategy. */
    private ICobolDbcsConverter mCobolDbcsConverter;
    
    /** Cobol compiler parameters. */
    private CobolContext mCobolContext;

    /**
     * Constructs a simple converters set.
     */
    public CobolSimpleConverters() {
        this(new CobolContext());
    }

    /**
     * Constructs a simple converters set.
     * 
     * @param cobolContext current Cobol compiler parameters
     */
    public CobolSimpleConverters(final CobolContext cobolContext) {
        mCobolContext = cobolContext;
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
        setCobolDbcsConverter(
                new CobolDbcsSimpleConverter(cobolContext));
    }

    /**
     * @return Returns the cobolBinaryConverter.
     */
    public ICobolBinaryConverter getCobolBinaryConverter() {
        return mCobolBinaryConverter;
    }

    /**
     * @param cobolBinaryConverter The cobolBinaryConverter to set.
     */
    public void setCobolBinaryConverter(
            final ICobolBinaryConverter cobolBinaryConverter) {
        mCobolBinaryConverter = cobolBinaryConverter;
    }

    /**
     * @return Returns the cobolDoubleConverter.
     */
    public ICobolDoubleConverter getCobolDoubleConverter() {
        return mCobolDoubleConverter;
    }

    /**
     * @param cobolDoubleConverter The cobolDoubleConverter to set.
     */
    public void setCobolDoubleConverter(
            final ICobolDoubleConverter cobolDoubleConverter) {
        mCobolDoubleConverter = cobolDoubleConverter;
    }

    /**
     * @return Returns the cobolFloatConverter.
     */
    public ICobolFloatConverter getCobolFloatConverter() {
        return mCobolFloatConverter;
    }

    /**
     * @param cobolFloatConverter The cobolFloatConverter to set.
     */
    public void setCobolFloatConverter(
            final ICobolFloatConverter cobolFloatConverter) {
        mCobolFloatConverter = cobolFloatConverter;
    }

    /**
     * @return Returns the cobolOctetStreamConverter.
     */
    public ICobolOctetStreamConverter getCobolOctetStreamConverter() {
        return mCobolOctetStreamConverter;
    }

    /**
     * @param cobolOctetStreamConverter The cobolOctetStreamConverter to set.
     */
    public void setCobolOctetStreamConverter(
            final ICobolOctetStreamConverter cobolOctetStreamConverter) {
        mCobolOctetStreamConverter = cobolOctetStreamConverter;
    }

    /**
     * @return Returns the cobolPackedDecimalConverter.
     */
    public ICobolPackedDecimalConverter getCobolPackedDecimalConverter() {
        return mCobolPackedDecimalConverter;
    }

    /**
     * @param cobolPackedDecimalConverter The cobolPackedDecimalConverter
     * to set.
     */
    public void setCobolPackedDecimalConverter(
            final ICobolPackedDecimalConverter cobolPackedDecimalConverter) {
        mCobolPackedDecimalConverter = cobolPackedDecimalConverter;
    }

    /**
     * @return Returns the cobolStringConverter.
     */
    public ICobolStringConverter getCobolStringConverter() {
        return mCobolStringConverter;
    }

    /**
     * @param cobolStringConverter The cobolStringConverter to set.
     */
    public void setCobolStringConverter(
            final ICobolStringConverter cobolStringConverter) {
        mCobolStringConverter = cobolStringConverter;
    }

    /**
     * @return Returns the cobolZonedDecimalConverter.
     */
    public ICobolZonedDecimalConverter getCobolZonedDecimalConverter() {
        return mCobolZonedDecimalConverter;
    }

    /**
     * @param cobolZonedDecimalConverter The cobolZonedDecimalConverter to set.
     */
    public void setCobolZonedDecimalConverter(
            final ICobolZonedDecimalConverter cobolZonedDecimalConverter) {
        mCobolZonedDecimalConverter = cobolZonedDecimalConverter;
    }

    /**
     * @return Returns the cobolNationalConverter.
     */
    public ICobolNationalConverter getCobolNationalConverter() {
        return mCobolNationalConverter;
    }

    /**
     * @param cobolNationalConverter The cobolNationalConverter to set.
     */
    public void setCobolNationalConverter(
            final ICobolNationalConverter cobolNationalConverter) {
        mCobolNationalConverter = cobolNationalConverter;
    }

    /**
     * @return Returns the cobolDbcsConverter.
     */
    public ICobolDbcsConverter getCobolDbcsConverter() {
        return mCobolDbcsConverter;
    }

    /**
     * @param cobolDbcsConverter The cobolDbcsConverter to set.
     */
    public void setCobolDbcsConverter(
            final ICobolDbcsConverter cobolDbcsConverter) {
        mCobolDbcsConverter = cobolDbcsConverter;
    }

    /**
     * @return Returns the CobolContext.
     */
    public CobolContext getCobolContext() {
        return mCobolContext;
    }

    /**
     * Transmits the new COBOL context to each converter.
     * @param cobolContext The CobolContext to set.
     */
    public void setCobolContext(final CobolContext cobolContext) {
        mCobolContext = cobolContext;
        getCobolStringConverter().setCobolContext(cobolContext);
        getCobolZonedDecimalConverter().setCobolContext(cobolContext);
        getCobolPackedDecimalConverter().setCobolContext(cobolContext);
        getCobolBinaryConverter().setCobolContext(cobolContext);
        getCobolFloatConverter().setCobolContext(cobolContext);
        getCobolDoubleConverter().setCobolContext(cobolContext);
        getCobolOctetStreamConverter().setCobolContext(cobolContext);
        getCobolNationalConverter().setCobolContext(cobolContext);
    }
}
