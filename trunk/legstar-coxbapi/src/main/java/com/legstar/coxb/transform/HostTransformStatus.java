package com.legstar.coxb.transform;

/**
 * This provides feedback on a transformation operation.
 * 
 */
public class HostTransformStatus {

    /** The total number of host bytes processed. */
    private int _hostBytesProcessed;

    /**
     * @return the total number of host bytes processed
     */
    public int getHostBytesProcessed() {
        return _hostBytesProcessed;
    }

    /**
     * @param hostBytesProcessed the total number of host bytes processed to set
     */
    public void setHostBytesProcessed(final int hostBytesProcessed) {
        _hostBytesProcessed = hostBytesProcessed;
    }

}
