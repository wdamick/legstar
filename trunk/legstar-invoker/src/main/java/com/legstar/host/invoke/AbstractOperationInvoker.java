package com.legstar.host.invoke;

import com.legstar.messaging.LegStarAddress;

/**
 * Generic code shared by all operation invoker implementations.
 * <p/>
 * Typically Adapters will derive their operations from this class.
 *
 */
public class AbstractOperationInvoker {

    /** The operation name.*/
    private String mOperationName;

    /** Host program properties for operation. */
    private String  mProgramProperties;
    
    /** The host invoker. */
    private HostInvoker mInvoker;

    /** The invoker configuration file name. */
    private String mConfigFileName;

    /**
     * Operation invoker constructor.
     * @param configFileName host invoker configuration file name
     * @param operationName operation name
     * @param programProperties host program properties
     */
    public AbstractOperationInvoker(
            final String configFileName,
            final String operationName,
            final String programProperties) {
        mConfigFileName = configFileName;
        mOperationName = operationName;
        mProgramProperties = programProperties;
    }

    /** {@inheritDoc} */
    public String toString() {
        StringBuffer details = new StringBuffer();
        details.append("Operation=" + getOperationName());
        if (getHostInvoker() != null) {
            details.append("," + getHostInvoker().toString());
        }
        return details.toString();
    }

    /**
     * Creates a new host invoker either because there is none yet or because something
     * in the request supersedes the previous address parameters.
     * @param address the target host address
     * @return a host invoker
     * @throws HostInvokerException if invoker cannot be created
     */
    public HostInvoker getHostInvoker(final LegStarAddress address) throws HostInvokerException {
        if (mInvoker == null || !mInvoker.getAddress().equals(address)) {
            mInvoker = HostInvokerFactory.createHostInvoker(
                    getConfigFileName(), address, getProgramProperties());
        }
        return mInvoker;
    }

    /**
     * @return the current host invoker
     */
    public HostInvoker getHostInvoker() {
        return mInvoker;
    }

    /**
     * @return the invoker configuration file name
     */
    public String getConfigFileName() {
        return mConfigFileName;
    }

    /**
     * @param configFileName the invoker configuration file name to set
     */
    public void setConfigFileName(final String configFileName) {
        mConfigFileName = configFileName;
    }


    /**
     * @return the host program properties for operation
     */
    public String getProgramProperties() {
        return mProgramProperties;
    }

    /**
     * @param programProperties the host program properties for operation to set
     */
    public void setProgramProperties(final String programProperties) {
        mProgramProperties = programProperties;
    }

    /**
     * @return the operation name
     */
    public String getOperationName() {
        return mOperationName;
    }

    /**
     * @param operationName the operation name to set
     */
    public void setOperationName(final String operationName) {
        mOperationName = operationName;
    }

}
