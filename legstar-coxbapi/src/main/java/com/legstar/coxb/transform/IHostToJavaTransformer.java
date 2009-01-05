package com.legstar.coxb.transform;

/**
 * Host to Java transformers offer the capability to turn a raw mainframe byte array
 * to a java data object.
 *
 */
public interface IHostToJavaTransformer extends IHostTransformer {

    /**
     * Transforms host data to java with a specific host character set.
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param hostCharset the host character set
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T  transform(final byte[] hostData, final String hostCharset) throws HostTransformException;

    /**
     * Transforms host data to java.
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    < T > T transform(final byte[] hostData) throws HostTransformException;

}
