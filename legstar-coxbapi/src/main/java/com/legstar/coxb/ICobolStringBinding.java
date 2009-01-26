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
 * This interface groups methods that are common to all cobol string elements
 * (PIC X).
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolStringBinding extends ICobolBinding {

    /**
     * Returns this element value.
     * @return Element value
     * @throws HostException if value cannot be retrieved
     */
    String getStringValue() throws HostException;


    /**
     * Sets the element value.
     * @param value Value to set
     * @throws HostException if value cannot be set
     */
    void setStringValue(String value) throws HostException;

}
