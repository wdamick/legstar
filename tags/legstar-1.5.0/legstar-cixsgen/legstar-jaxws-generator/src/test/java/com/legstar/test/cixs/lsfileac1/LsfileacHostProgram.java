package com.legstar.test.cixs.lsfileac1;

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
        getOutputContainers().add(new HostContainer("ReplyData", 7905));
    }

}
