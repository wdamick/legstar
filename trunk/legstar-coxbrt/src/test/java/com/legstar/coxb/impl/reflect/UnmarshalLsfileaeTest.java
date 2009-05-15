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
package com.legstar.coxb.impl.reflect;

import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

/**
 * Test LSFILEAE.
 *
 */
public class UnmarshalLsfileaeTest extends AbstractTestUnmarshal {

    /**
     * Unmarshal Lsfileae.
     */
    public void testLsfileae() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                LsfileaeCases.getFactory(),
                LsfileaeCases.getHostBytesHex(),
                LsfileaeCases.getJavaObject());
        LsfileaeCases.checkJavaObject(dfhcommarea);
    }

}
