package com.legstar.test.cixs.fixarnum;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class FixarnumHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public FixarnumHostProgram() {
        setName("FIXARNUM");
        setMaxDataLength(78);
        setDataLength(78);
    }

}
