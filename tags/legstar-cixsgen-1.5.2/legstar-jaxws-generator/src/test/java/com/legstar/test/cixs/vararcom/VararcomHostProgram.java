package com.legstar.test.cixs.vararcom;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class VararcomHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public VararcomHostProgram() {
        setName("VARARCOM");
        setMaxDataLength(1752);
        setDataLength(1752);
    }

}
