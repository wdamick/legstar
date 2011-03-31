package com.legstar.test.cixs.dplarcht;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class DplarchtHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public DplarchtHostProgram() {
        setName("DPLARCHT");
        setMaxDataLength(32025);
        setDataLength(32025);
    }

}
