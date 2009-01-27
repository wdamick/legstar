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
package com.legstar.host.server;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import junit.framework.TestCase;

/**
 * Test the configuration reader.
 *
 */
public class ConfigReaderTest extends TestCase {

    /** Endpoint configuration. */
    private static final String HOST_ENDPOINT_CFG = "hostEndPoints.hostEndPoint";

    /**
     * Basic configuration read.
     * @throws ConfigurationException if read fails
     */
    public void testReadConfig() throws ConfigurationException {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        List < ? > endpoints = config.configurationsAt(HOST_ENDPOINT_CFG);
        for (Iterator < ? > it = endpoints.iterator(); it.hasNext();) {
            HierarchicalConfiguration sub = (HierarchicalConfiguration) it.next();
            assertEquals("TheOtherMainframe", sub.getString("[@name]"));
            assertEquals("mainframe", sub.getString("hostIPAddress"));
        }
    }

    /**
     * Read using XPath expression.
     * @throws ConfigurationException if read fails
     */
    public void testReadConfigXPath() throws ConfigurationException {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        config.setExpressionEngine(new XPathExpressionEngine());

        List < ? > endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtherMainframe']");
        assertTrue(!endpoints.isEmpty());
        for (Iterator < ? > it = endpoints.iterator(); it.hasNext();) {
            HierarchicalConfiguration sub = (HierarchicalConfiguration) it.next();
            assertEquals("TheOtherMainframe", sub.getString("@name"));
            assertEquals("mainframe", sub.getString("hostIPAddress"));
        }
    }

    /**
     * Test reading a non existing element.
     * @throws ConfigurationException if test fails
     */
    public void testNotFoundElement() throws ConfigurationException {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        config.setExpressionEngine(new XPathExpressionEngine());

        List < ? > endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtterrMainframe']");
        assertTrue(endpoints.isEmpty());
    }

    /**
     * Test reading the configuration directly.
     * @throws ConfigurationException if reading fails
     */
    public void testReadConfigXPathDirect() throws ConfigurationException {
        HierarchicalConfiguration config = Util.getCombinedConfiguration();
        config.setExpressionEngine(new XPathExpressionEngine());

        List < ? >  endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtherMainframe']");
        assertTrue(!endpoints.isEmpty());
        HierarchicalConfiguration endpoint = (HierarchicalConfiguration) endpoints.get(0);
        assertEquals("TheOtherMainframe", endpoint.getString("@name"));
        assertEquals("mainframe", endpoint.getString("hostIPAddress"));
    }

}
