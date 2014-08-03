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
package com.legstar.http.client;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.test.connection.client.AbstractConnectionPooledMeteringTest;

/**
 * Test HTTP transport with LegStar Messaging and pooling engine.
 *
 */
public class CicsTs23PooledMeteringTest extends AbstractConnectionPooledMeteringTest {

    /** {@inheritDoc} */
    public PoolingEngineConfig getPoolingEngineConfig() {
        return AbstractHttpConnectionTester.getCicsTs23PoolingEngineConfig();
    }

}
