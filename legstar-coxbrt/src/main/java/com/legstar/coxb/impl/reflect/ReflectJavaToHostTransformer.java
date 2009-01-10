package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.transform.AbstractJavaToHostTransformer;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;

/**
 * This implementation of a java to host transformer dynamically binds to
 * a complex object using reflection.
 * The complex object must be a JAXB type with COBOL annotations.
 *
 */
public class ReflectJavaToHostTransformer extends AbstractJavaToHostTransformer {
    
    /**
     * The JAXB type factory to use for the JAXB object type.
     */
    private Object mObjectFactory;

    /**
     * The JAXB type class.
     */
    private Class < ? > mJaxbClass;

    /**
     * Construct a transformer for a particular JAXB type.
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws ReflectBindingException if JAXB type has no annotations or cannot be 
     *  located from the classpath
     */
    public ReflectJavaToHostTransformer(
            final String jaxbPackageName,
            final String jaxbType) throws ReflectBindingException {
        try {
            JAXBElementDescriptor jaxbElementDescriptor =
                new JAXBElementDescriptor(jaxbPackageName, jaxbType);
            mObjectFactory = jaxbElementDescriptor.createObjectFactory();
            mJaxbClass = jaxbElementDescriptor.loadJaxbClass();
        } catch (JAXBAnnotationException e) {
            throw new ReflectBindingException(e);
        } catch (ClassNotFoundException e) {
            throw new ReflectBindingException(e);
        }
    }
    
    /** {@inheritDoc} */
    public ICobolComplexBinding getBinding() throws CobolBindingException {
        try {
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    getObjectFactory(), getJaxbClass());
            return ccem;
        } catch (ReflectBindingException e) {
            throw new CobolBindingException(e);
        }
    }

    /**
     * @return the JAXB type factory to use for the JAXB object type
     */
    public Object getObjectFactory() {
        return mObjectFactory;
    }

    /**
     * @return the JAXB type class
     */
    public Class < ? > getJaxbClass() {
        return mJaxbClass;
    }

}
