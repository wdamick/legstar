package com.legstar.cixs.gen.ant;

import com.legstar.coxb.host.HostException;
import com.legstar.coxb.transform.IHostTransformers;
import com.legstar.coxb.util.BindingUtil;

/**
 * Provides cixs generators with convenience methods. The class can be
 * passed as an instance to the velocity engine and used by templates.
 * 
 */
public class CixsHelper {

    /**
     * Return the maximum host byte length for a COBOL-annotated JAXB class.
     * <p/>
     * Here we assume binding classes were generated so we can use their ability
     * to deliver the byte length without going through a JAXB annotation
     * introspection.
     * 
     * @param jaxbPackageName the JAXB package name
     * @param jaxbClassName the JAXB class name of the object for which byte
     *            length must be returned
     * @return the byte length
     * @throws HostException if byte length calculation failed
     */
    public long getByteLength(
            final String jaxbPackageName,
            final String jaxbClassName) throws HostException {
        IHostTransformers tf = BindingUtil.newTransformers(jaxbPackageName,
                jaxbClassName);
        return tf.getHostToJava().getByteLength();
    }

    /**
     * A mere wrapper on the static <code>JaxbUtil.getJavaClassName</code>.
     * 
     * @param jaxbPackage the JAXB package name from which a java class name
     *            is to be derived
     * @param jaxbTypeName the JAXB type name from which a java class name
     *            is to be derived
     * @return a class name (including package name) that the JAXB class
     *         is hiding or the JAXB class itself if it is not hiding a POJO.
     * @throws HostException if deriving a java class name fails
     */
    public String getJavaClassName(
            final String jaxbPackage,
            final String jaxbTypeName) throws HostException {
        return BindingUtil.getJavaClassName(jaxbPackage, jaxbTypeName);
    }

}
