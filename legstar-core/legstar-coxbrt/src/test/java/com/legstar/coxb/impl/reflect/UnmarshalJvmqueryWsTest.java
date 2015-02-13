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
package com.legstar.coxb.impl.reflect;

import com.legstar.test.coxb.JvmqueryWsCases;
import com.legstar.test.coxb.ws.jvmquery.QueryJvmResponse;

/**
 * Test JVMQUERY.
 *
 */
public class UnmarshalJvmqueryWsTest extends AbstractTestUnmarshal {

    /**
     * Unmarshal JvmqueryWs.
     */
    public void testJvmqueryWs() {
        QueryJvmResponse queryJvmResponse = (QueryJvmResponse) convert(
                JvmqueryWsCases.getFactory(),
                JvmqueryWsCases.getHostBytesHex(),
                JvmqueryWsCases.getJavaObject());
        JvmqueryWsCases.checkJavaObject(queryJvmResponse);
    }

}
