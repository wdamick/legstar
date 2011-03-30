package com.legstar.test.cixs.lsfileac;

import com.legstar.host.invoke.model.HostContainer;
import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class LsfileacHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public LsfileacHostProgram() {
        setName("LSFILEAC");
        setChannelName("LSFILEAC-CHANNEL");
        getInputContainers().add(new HostContainer("QueryData", 48));
        getInputContainers().add(new HostContainer("QueryLimit", 10));
        getOutputContainers().add(new HostContainer("ReplyData", 7905));
        getOutputContainers().add(new HostContainer("ReplyStatus", 151));
    }

}
