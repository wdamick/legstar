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

import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.host.AbstractTester;

/**
 * Test the default configuration.
 *
 */
public class DefaultConfigTest extends AbstractTester {
    
    /**
     * Check that the default configuration is loaded.
     * @throws HostAccessStrategyException if test fails
     */
    public void testDefaultEndPoint() throws HostAccessStrategyException {
        XMLConfiguration generalConfig;
        try {
            generalConfig = new XMLConfiguration(CONFIG_FILE);
        } catch (ConfigurationException e) {
            throw new HostAccessStrategyException(e);
        }
        generalConfig.setExpressionEngine(new XPathExpressionEngine());

        String strXPath = HOST_ENDPOINT_CFG;
        List < ? >  endpoints = generalConfig.configurationsAt(strXPath);
        if (endpoints == null || endpoints.isEmpty()) {
            throw new HostAccessStrategyException(
            "No default endpoint is defined.");
        }
        HierarchicalConfiguration sub = (HierarchicalConfiguration) endpoints.get(0);
        assertEquals("TheMainframe", sub.getString("@name"));


    }

}
