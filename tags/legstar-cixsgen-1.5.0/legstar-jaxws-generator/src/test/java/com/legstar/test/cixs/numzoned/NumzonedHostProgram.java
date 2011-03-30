package com.legstar.test.cixs.numzoned;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class NumzonedHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public NumzonedHostProgram() {
        setName("NUMZONED");
        setMaxDataLength(13);
        setDataLength(13);
    }

}
