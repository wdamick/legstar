package com.legstar.coxb.transform;

/**
 * Java to Host transformers offer the capability to turn a java data object
 * to raw mainframe byte arrays.
 *
 */
public interface IJavaToHostTransformer extends IHostTransformer {

    /**
     * Transforms java to host data with a specific host character set.
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Object valueObject, final String hostCharset) throws HostTransformException;

    /**
     * Transforms java to host data.
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    byte[] transform(final Object valueObject) throws HostTransformException;

}
