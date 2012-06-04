package com.legstar.test.cixs.binpkdus;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class BinpkdusHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public BinpkdusHostProgram() {
        setName("BINPKDUS");
        setMaxDataLength(44);
        setDataLength(44);
    }

}
