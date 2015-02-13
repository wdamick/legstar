/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

/**
 * This interface groups methods that are common to all array of numeric
 * elements.
 * 
 * @author Fady Moussallam
 * 
 */
public interface ICobolArrayNumericBinding extends ICobolArrayBinding {

    /**
     * @return the numeric list as a List of Bytes
     */
    List < Byte > getByteList();

    /**
     * @param list the numeric list as a List of Bytes to set
     */
    void setByteList(List < Byte > list);

    /**
     * @return the numeric list as a List of Shorts
     */
    List < Short > getShortList();

    /**
     * @param list the numeric list as a List of Shorts to set
     */
    void setShortList(List < Short > list);

    /**
     * @return the numeric list as a List of Integers
     */
    List < Integer > getIntegerList();

    /**
     * @param list the numeric list as a List of Integers to set
     */
    void setIntegerList(List < Integer > list);

    /**
     * @return the numeric list as a List of Longs
     */
    List < Long > getLongList();

    /**
     * @param list the numeric list as a List of Longs to set
     */
    void setLongList(List < Long > list);

    /**
     * @return the numeric list as a List of BigDecimals
     */
    List < BigDecimal > getBigDecimalList();

    /**
     * @param list the numeric list as a List of BigDecimals to set
     */
    void setBigDecimalList(List < BigDecimal > list);

    /**
     * @return the numeric list as a List of BigIntegers
     */
    List < BigInteger > getBigIntegerList();

    /**
     * @param list the numeric list as a List of BigIntegers to set
     */
    void setBigIntegerList(List < BigInteger > list);
}
