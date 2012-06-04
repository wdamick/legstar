package com.legstar.test.cixs.charsets;

import com.legstar.host.invoke.model.HostProgram;

/**
 * Target host program attributes.
 *
 */
public class CharsetsHostProgram extends HostProgram {
    
    /**
     * Setup attributes values.
     */
    public CharsetsHostProgram() {
        setName("CHARSETS");
        setMaxDataLength(160);
        setDataLength(160);
    }

}
