package com.legstar.test.cixs.doublmix;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class DoublmixHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public DoublmixHostProgram() {
        setName("DOUBLMIX");
        setMaxDataLength(48);
        setDataLength(48);
    }

}
