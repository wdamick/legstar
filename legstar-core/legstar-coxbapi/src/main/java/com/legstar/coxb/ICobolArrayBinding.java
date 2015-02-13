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

import com.legstar.coxb.host.HostException;

/**
 * This interface groups methods that are common to all arrays.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolArrayBinding extends ICobolBinding {
    
    /**
     * Fixed size arrays return the maximum number of occurrences while variable
     * size arrays dynamically compute their dimension.
     * @return the current number of items
     * @throws HostException if count cannot be computed
     */
    int getCurrentOccurs() throws HostException;
    
    /**
     * In COBOL, arrays are collections of items with a fixed size.
     * This method returns an individual item host byte size.
     * @return Cobol array individual item length in bytes.
     */
    int getItemByteLength();

    /**
     * Set the individual item host byte size.
     * @param itemByteLength Cobol array individual item length in bytes.
     */
    void setItemByteLength(int itemByteLength);

    /**
     * The method calculates the exact host byte length for one
     * individual item of this Cobol binding.
     * @return the host byte length of an individual item of this binding
     */
    int calcItemByteLength();
}
