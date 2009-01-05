package com.legstar.coxb.transform;


/**
 * A generic class that provides transformer capabilities for a given structure.
 * <p/>
 * A structure maps to a java class and a COBOL structure. This class provides 
 * transformer methods for host to java and java to host. It does not implement
 * the transformers though, it acts as a container.
 * <p/>
 * Classes derived from this one will typically implement a constructor that
 * creates the directional transformers, java to host and host to java.
 *
 */
public abstract class AbstractTransformerProvider {

    /** Transformer that turns a java data object into host data. */
    private IJavaToHostTransformer mJavaToHost;

    /** Transformer that turns host data into a java data object. */
    private IHostToJavaTransformer mHostToJava;
    
    /**
     * No arg constructor. Caller is responsible for setting the internal transformers.
     */
    public AbstractTransformerProvider() {
        
    }
    
    /**
     * Creates a provider with its directional transformers.
     * @param javaToHost java to host transformer
     * @param hostToJava host to java transformer
     */
    public AbstractTransformerProvider(
            final IJavaToHostTransformer javaToHost,
            final IHostToJavaTransformer hostToJava) {
        mJavaToHost = javaToHost;
        mHostToJava = hostToJava;
    }
    /**
     * Transforms java data object to host data with a specific host character set.
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject, final String hostCharset) throws HostTransformException {
        return getJavaToHost().transform(valueObject, hostCharset);
    }
    
    /**
     * Transforms java data object to host data.
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject) throws HostTransformException {
        return getJavaToHost().transform(valueObject);
    }
    
    /**
     * Transforms host data to java data object with a specific host character set.
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param hostCharset the host character set
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    @SuppressWarnings("unchecked")
    public < T > T  toJava(final byte[] hostData, final String hostCharset) throws HostTransformException {
        return (T) getHostToJava().transform(hostData, hostCharset);
    }

    /**
     * Transforms host data to java data object.
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    @SuppressWarnings("unchecked")
    public < T > T toJava(final byte[] hostData) throws HostTransformException {
        return (T) getHostToJava().transform(hostData);
    }

    /**
     * @return the transformer that turns a java data object into host data
     */
    public IJavaToHostTransformer getJavaToHost() {
        return mJavaToHost;
    }

    /**
     * @param javaToHost the the transformer that turns a java data object into host data to set
     */
    public void setJavaToHost(
            final IJavaToHostTransformer javaToHost) {
        mJavaToHost = javaToHost;
    }

    /**
     * @return the transformer that turns host data into a java data object
     */
    public IHostToJavaTransformer getHostToJava() {
        return mHostToJava;
    }

     /**
     * @param hostToJava the transformer that turns host data into a java data object to set
     */
    public void setHostToJava(
            final IHostToJavaTransformer hostToJava) {
        mHostToJava = hostToJava;
    }

}
