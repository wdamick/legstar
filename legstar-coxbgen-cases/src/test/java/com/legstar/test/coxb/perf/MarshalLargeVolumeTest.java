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
package com.legstar.test.coxb.perf;

import com.legstar.coxb.host.HostException;

/**
 * Test marshaling a large workload. 
 *
 */
public class MarshalLargeVolumeTest extends AbstractMarshalVolume {

    /**
     * Create the test case.
     */
    public MarshalLargeVolumeTest() {
        super(100, 500, 32025, 500L);
    }

    /**
     * Run the test case.
     * @throws HostException if test fails
     */
   public void testMarshal() throws HostException {
        marshal();
    }
}
