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
package com.legstar.host.access;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.coxb.host.HostData;
import com.legstar.host.AbstractTester;
import com.legstar.messaging.LegStarRequest;
import com.legstar.test.coxb.LsfileaeCases;

/**
 * Direct (non pooled) host access.
 *
 */
public class DirectHostAccessStrategyTest extends AbstractTester {

    /**
     * Check what happens when factory is not found.
     */
    public void testConstructorNoFactory() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config1.xml", "TheMainframe");
            new DirectHostAccessStrategy(endpointConfig);
            fail("testConstructorNoFactory failed ");
        } catch (HostAccessStrategyException e) {
            assertEquals("org.apache.commons.configuration.ConfigurationException:"
                    + " java.lang.ClassNotFoundException:"
                    + " com.legstar.truc.much.CicsSocketConnectionFactory", e.getMessage());
        } catch (ConfigurationException e) {
            fail("testConstructorNoFactory failed ");
        }
    }

    /**
     * Check normal loading.
     */
    public void testConstructor() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
            DirectHostAccessStrategy dha = new DirectHostAccessStrategy(endpointConfig);
            assertTrue(dha != null);
        } catch (ConfigurationException e) {
            fail("testConstructorNoFactory failed ");
        } catch (HostAccessStrategyException e) {
            fail("testConstructor failed " + e);
        }
    }

    /**
     * Try invoking the mainframe.
     */
    public void testInvoke() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
            DirectHostAccessStrategy dha = new DirectHostAccessStrategy(endpointConfig);
            LegStarRequest request = createLsfileaeRequest();
            dha.invoke(request);
            assertEquals(LsfileaeCases.getHostBytesHexReply100(), 
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (ConfigurationException e) {
            fail("testConstructorNoFactory failed ");
        } catch (HostAccessStrategyException e) {
            fail("testInvoke failed " + e);
        }
    }
}
