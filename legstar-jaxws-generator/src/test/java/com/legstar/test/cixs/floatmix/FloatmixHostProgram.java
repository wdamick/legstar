package com.legstar.test.cixs.floatmix;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class FloatmixHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public FloatmixHostProgram() {
        setName("FLOATMIX");
        setMaxDataLength(24);
        setDataLength(24);
    }

}
