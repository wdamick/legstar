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
package com.legstar.coxb.transform;

import com.legstar.coxb.ICobolComplexBinding;

/**
 * This provides feedback on a transformation operation.
 * 
 */
public class HostTransformStatus {

    /** The total number of host bytes processed. */
    private int _hostBytesProcessed;

    /** The root binding that was used for the transformation. */
    private ICobolComplexBinding _binding;

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

    /**
     * @return the root binding that was used for the transformation
     */
    public ICobolComplexBinding getBinding() {
        return _binding;
    }

    /**
     * @param binding the root binding that was used for the transformation to
     *            set
     */
    public void setBinding(final ICobolComplexBinding binding) {
        _binding = binding;
    }

}
