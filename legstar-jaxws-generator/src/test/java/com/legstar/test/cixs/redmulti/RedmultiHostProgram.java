package com.legstar.test.cixs.redmulti;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class RedmultiHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public RedmultiHostProgram() {
        setName("REDMULTI");
        setMaxDataLength(206);
        setDataLength(206);
    }

}
