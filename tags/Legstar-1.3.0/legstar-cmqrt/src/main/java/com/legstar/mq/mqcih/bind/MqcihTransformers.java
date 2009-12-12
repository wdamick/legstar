package com.legstar.mq.mqcih.bind;

import com.legstar.coxb.transform.AbstractTransformers;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.mq.mqcih.Mqcih;

/**
 * Transformer provider for Mqcih java data object.
 *
 */
public class MqcihTransformers extends AbstractTransformers {

    /**
     * Create a set of directional transformers.
     */
    public MqcihTransformers() {
        super(new MqcihJavaToHostTransformer(),
                new MqcihHostToJavaTransformer());
    }

    /**
     * Transforms java data object to host data with a specific host character set.
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
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Object valueObject)
            throws HostTransformException {
        return getJavaToHost().transform(valueObject);
    }

    /**
     * Transforms host data to java data object with a specific host character set.
     * @param hostData a byte array containing host data
     * @param hostCharset the host character set
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    public Mqcih toJava(final byte[] hostData, final String hostCharset)
            throws HostTransformException {
        return getHostToJava().transform(hostData, hostCharset);
    }

    /**
     * Transforms host data to java data object.
     * @param hostData a byte array containing host data
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    public Mqcih toJava(final byte[] hostData)
            throws HostTransformException {
        return (Mqcih) getHostToJava().transform(hostData);
    }
}
