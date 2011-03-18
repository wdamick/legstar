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
package com.legstar.coxb.convert;

import com.legstar.coxb.CobolContext;

/**
 * This interface is implemented by converter sets. A converter set is a family
 * of converters each specializing in a COBOL data type.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolConverters {

    /**
     * @return Returns the CobolContext.
     */
    CobolContext getCobolContext();

    /**
     * @param cobolContext The CobolContext to set.
     */
    void setCobolContext(final CobolContext cobolContext);

    
    /**
     * @return Returns the cobolBinaryConverter.
     */
    ICobolBinaryConverter getCobolBinaryConverter();

    /**
     * @param cobolBinaryConverter The cobolBinaryConverter to set.
     */
    void setCobolBinaryConverter(final ICobolBinaryConverter cobolBinaryConverter);

    /**
     * @return Returns the cobolDoubleConverter.
     */
    ICobolDoubleConverter getCobolDoubleConverter();

    /**
     * @param cobolDoubleConverter The cobolDoubleConverter to set.
     */
    void setCobolDoubleConverter(final ICobolDoubleConverter cobolDoubleConverter);

    /**
     * @return Returns the cobolFloatConverter.
     */
    ICobolFloatConverter getCobolFloatConverter();

    /**
     * @param cobolFloatConverter The cobolFloatConverter to set.
     */
    void setCobolFloatConverter(final ICobolFloatConverter cobolFloatConverter);

    /**
     * @return Returns the cobolOctetStreamConverter.
     */
    ICobolOctetStreamConverter getCobolOctetStreamConverter();

    /**
     * @param cobolOctetStreamConverter The cobolOctetStreamConverter to set.
     */
    void setCobolOctetStreamConverter(final ICobolOctetStreamConverter cobolOctetStreamConverter);

    /**
     * @return Returns the cobolPackedDecimalConverter.
     */
    ICobolPackedDecimalConverter getCobolPackedDecimalConverter();

    /**
     * @param cobolPackedDecimalConverter The cobolPackedDecimalConverter
     * to set.
     */
    void setCobolPackedDecimalConverter(final ICobolPackedDecimalConverter cobolPackedDecimalConverter);

    /**
     * @return Returns the cobolStringConverter.
     */
    ICobolStringConverter getCobolStringConverter();

    /**
     * @param cobolStringConverter The cobolStringConverter to set.
     */
    void setCobolStringConverter(final ICobolStringConverter cobolStringConverter);

    /**
     * @return Returns the cobolZonedDecimalConverter.
     */
    ICobolZonedDecimalConverter getCobolZonedDecimalConverter();

    /**
     * @param cobolZonedDecimalConverter The cobolZonedDecimalConverter to set.
     */
    void setCobolZonedDecimalConverter(final ICobolZonedDecimalConverter cobolZonedDecimalConverter);

    /**
     * @return Returns the cobolNationalConverter.
     */
    ICobolNationalConverter getCobolNationalConverter();

    /**
     * @param cobolNationalConverter The cobolNationalConverter to set.
     */
    void setCobolNationalConverter(final ICobolNationalConverter cobolNationalConverter);

    /**
     * @return Returns the cobolDbcsConverter.
     */
    ICobolDbcsConverter getCobolDbcsConverter();

    /**
     * @param cobolDbcsConverter The cobolDbcsConverter to set.
     */
    void setCobolDbcsConverter(final ICobolDbcsConverter cobolDbcsConverter);

}
