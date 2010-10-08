/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

import java.util.List;

import com.legstar.coxb.host.HostException;

/**
 * This interface groups methods that are common to all cobol array octet
 * stream elements.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolArrayOctetStreamBinding extends ICobolArrayBinding {


    /**
     * @return Returns an ArrayList of byte arrays 
     * @throws HostException list cannot be created
     */
    List < byte[] > getByteArrayList() throws HostException;

    /**
     * Sets a list of byte arrays.
     * @param iArray set array to set
     * @throws HostException list cannot be set
     */
    void setByteArrayList(List < byte[] > iArray) throws HostException;

}
