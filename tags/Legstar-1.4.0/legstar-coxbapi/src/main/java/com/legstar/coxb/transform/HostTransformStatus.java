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
package com.legstar.coxb.transform;

/**
 * This provides feedback on a transformation operation.
 * 
 */
public class HostTransformStatus {

    /** The total number of host bytes processed. */
    private int _hostBytesProcessed;

    /**
     * @return the total number of host bytes processed
     */
    public int getHostBytesProcessed() {
        return _hostBytesProcessed;
    }

    /**
     * @param hostBytesProcessed the total number of host bytes processed to set
     */
    public void setHostBytesProcessed(final int hostBytesProcessed) {
        _hostBytesProcessed = hostBytesProcessed;
    }

}
