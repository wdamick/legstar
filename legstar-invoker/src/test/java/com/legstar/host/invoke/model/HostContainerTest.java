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
package com.legstar.host.invoke.model;

import junit.framework.TestCase;

/**
 * Test the HostContainer class.
 *
 */
public class HostContainerTest extends TestCase {
    
    /**
     * Check that class instantiates and serializes to string.
     */
    public void testInstentiation() {
        HostContainer hostContainer = new HostContainer();
        assertEquals(
                "{\"containerName\":null,"
                + "\"containerLength\":0}",
                hostContainer.toString());
        hostContainer.setName("krakatoa");
        assertEquals(
                "{\"containerName\":\"krakatoa\","
                + "\"containerLength\":0}",
                hostContainer.toString());
        hostContainer.setLength(18);
        assertEquals(
                "{\"containerName\":\"krakatoa\","
                + "\"containerLength\":18}",
                hostContainer.toString());
    }

}
