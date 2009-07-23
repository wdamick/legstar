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
