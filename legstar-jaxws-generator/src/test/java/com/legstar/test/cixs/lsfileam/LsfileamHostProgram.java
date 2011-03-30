package com.legstar.test.cixs.lsfileam;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class LsfileamHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public LsfileamHostProgram() {
        setName("LSFILEAM");
        setMaxDataLength(8056);
        setDataLength(58);
    }

}
