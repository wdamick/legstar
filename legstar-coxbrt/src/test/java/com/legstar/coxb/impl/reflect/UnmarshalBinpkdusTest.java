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

import com.legstar.test.coxb.BinpkdusCases;
import com.legstar.test.coxb.binpkdus.Dfhcommarea;

/**
 * Test BINPKDUS.
 *
 */
public class UnmarshalBinpkdusTest extends AbstractTestUnmarshal {

    /**
     * Unmarshal Binpkdus.
     */
    public final void testBinpkdus() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                BinpkdusCases.getFactory(),
                BinpkdusCases.getHostBytesHex(),
                BinpkdusCases.getJavaObject());
        BinpkdusCases.checkJavaObject(dfhcommarea);
    }

}
