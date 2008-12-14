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
package com.legstar.coxb;

import com.legstar.coxb.host.HostException;

import java.math.BigDecimal;
import java.util.List;

/**
 * This interface groups methods that are common to double array elements.
 * 
 * @author Fady Moussallam
 * 
 */
public interface ICobolArrayDoubleBinding extends ICobolArrayBinding {

    /**
     * @return Returns an ArrayList of Doubles 
     * @throws HostException list cannot be created
     */
    List < Double > getDoubleList() throws HostException;

    /**
     * Sets a list of Doubles.
     * @param iArray set array to set
     * @throws HostException list cannot be set
     */
    void setDoubleList(List < Double > iArray) throws HostException;

    /**
     * @return Returns an ArrayList of BigDecimals 
     * @throws HostException list cannot be created
     */
    List < BigDecimal > getBigDecimalList() throws HostException;

    /**
     * Sets a list of BigDecimals.
     * @param iArray set array to set
     * @throws HostException list cannot be set
     */
    void setBigDecimalList(List < BigDecimal > iArray) throws HostException;
}
