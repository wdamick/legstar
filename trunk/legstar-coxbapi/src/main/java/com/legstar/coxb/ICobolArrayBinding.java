/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
     * Fixed size arrays return the maximum number of occurences while variable
     * size arrays dynamically compute their dimension.
     * @return the current number of items
     * @throws HostException if count cannot be computed
     */
    int getCurrentOccurs() throws HostException;
}
