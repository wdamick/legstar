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
        setChannel("LSFILEAC-CHANNEL");
        getInContainers().add(new HostContainer("QueryData", 48));
        getOutContainers().add(new HostContainer("ReplyData", 7905));
    }

}
