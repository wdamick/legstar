package com.legstar.coxb.transform;

/**
 * A transformers class where individual transformers use reflection
 * on JAXB classes to perform the actual binding.
 *
 */
public class DplarchtTransformers extends AbstractTransformers {

    
    /**
     * Creates a transformer for each direction at construction time.
     */
    public DplarchtTransformers() {
        super(new JavaToHostDplarchtTransformer(),
                new HostToJavaDplarchtTransformer());
    }

    /** {@inheritDoc}*/
    public byte[] toHost(final Object valueObject, final String hostCharset)
            throws HostTransformException {
        return getJavaToHost().transform(valueObject, hostCharset);
    }

    /** {@inheritDoc}*/
    public byte[] toHost(final Object valueObject) throws HostTransformException {
        return getJavaToHost().transform(valueObject);
    }

    /** {@inheritDoc}*/
    public Object toJava(final byte[] hostData, final String hostCharset)
            throws HostTransformException {
        return getHostToJava().transform(hostData, hostCharset);
    }

    /** {@inheritDoc}*/
    public Object toJava(final byte[] hostData) throws HostTransformException {
        return getHostToJava().transform(hostData);
    }

}
