/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.transform;

/**
 * A generic class that provides transformer capabilities for a given structure.
 * <p/>
 * A structure maps to a java class and a COBOL structure. This class does not
 * implement the transformers, it acts as a container.
 * <p/>
 * Classes derived from this one will typically implement a constructor that
 * creates the directional transformers, java to host and host to java.
 * 
 */
public abstract class AbstractTransformers implements IHostTransformers {

    /** Transformer that turns a java data object into host data. */
    private IJavaToHostTransformer mJavaToHost;

    /** Transformer that turns host data into a java data object. */
    private IHostToJavaTransformer mHostToJava;

    /**
     * No arg constructor. Caller is responsible for setting the internal
     * transformers.
     */
    public AbstractTransformers() {

    }

    /**
     * Creates a provider with its directional transformers.
     * 
     * @param javaToHost java to host transformer
     * @param hostToJava host to java transformer
     */
    public AbstractTransformers(
            final IJavaToHostTransformer javaToHost,
            final IHostToJavaTransformer hostToJava) {
        mJavaToHost = javaToHost;
        mHostToJava = hostToJava;
    }

    /**
     * @return the transformer that turns a java data object into host data
     */
    public IJavaToHostTransformer getJavaToHost() {
        return mJavaToHost;
    }

    /**
     * @param javaToHost the the transformer that turns a java data object into
     *            host data to set
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
     * @param hostToJava the transformer that turns host data into a java data
     *            object to set
     */
    public void setHostToJava(
            final IHostToJavaTransformer hostToJava) {
        mHostToJava = hostToJava;
    }

    /**
     * Transforms java data object to host data with a specific host character
     * set.
     * 
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject, final String hostCharset)
            throws HostTransformException {
        return getJavaToHost().transform(valueObject, hostCharset);
    }

    /**
     * Transforms java data object to host data.
     * 
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject)
            throws HostTransformException {
        return getJavaToHost().transform(valueObject);
    }

    /**
     * Transforms java data object to host data with a specific host character
     * set.
     * 
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject, final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        return getJavaToHost().transform(valueObject, hostCharset, status);
    }

    /**
     * Transforms java data object to host data.
     * 
     * @param valueObject a java value object
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject,
            final HostTransformStatus status)
            throws HostTransformException {
        return getJavaToHost().transform(valueObject, status);
    }

    /**
     * {@inheritDoc} Provide a default implementation for backward
     * compatibility.
     */
    public Object toJava(final byte[] hostData, final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        return getHostToJava().transform(hostData, hostCharset, status);
    }

    /**
     * {@inheritDoc} Provide a default implementation for backward
     * compatibility.
     */
    public Object toJava(final byte[] hostData, final HostTransformStatus status)
            throws HostTransformException {
        return getHostToJava().transform(hostData, status);
    }

    /**
     * {@inheritDoc} Provide a default implementation for backward
     * compatibility.
     */
    public Object toJava(final byte[] hostData, final int offset,
            final String hostCharset)
            throws HostTransformException {
        return getHostToJava().transform(hostData, offset, hostCharset);
    }

    /**
     * {@inheritDoc} Provide a default implementation for backward
     * compatibility.
     */
    public Object toJava(final byte[] hostData, final int offset)
            throws HostTransformException {
        return getHostToJava().transform(hostData, offset);
    }

    /**
     * {@inheritDoc} Provide a default implementation for backward
     * compatibility.
     */
    public Object toJava(final byte[] hostData, final int offset,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        return getHostToJava().transform(hostData, offset, hostCharset, status);
    }

    /**
     * {@inheritDoc} Provide a default implementation for backward
     * compatibility.
     */
    public Object toJava(final byte[] hostData, final int offset,
            final HostTransformStatus status)
            throws HostTransformException {
        return getHostToJava().transform(hostData, offset, status);
    }

}
