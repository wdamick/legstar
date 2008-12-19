package com.legstar.coxb.util;

/**
 * Utilities taht are common to the binding API.
 *
 */
public final class Utils {
    
    /**
     * A utility class.
     */
    private Utils() {
        
    }

    /**
     * NOTE: This code is already in com.legstar.util.JaxbUtil. But we dont
     * want to create a depency on the coxb implementation here.
     * TODO find a way to share this code.
     * Rather than using the Class.forName mechanism, this uses
     * Thread.getContextClassLoader instead. In a Servlet context such as
     * Tomcat, this allows JAXB classes for instance to be loaded from the
     * web application (webapp) location while this code might have been
     * loaded from shared/lib.
     * If Thread.getContextClassLoader fails to locate the class then we
     * give a last chance to Class.forName.
     * @param className the class name to load
     * @return the class
     * @throws ClassNotFoundException if class is not accessible from this
     * thread loader
     */
    public static Class < ? > loadClass(
            final String className) throws ClassNotFoundException {
        Class < ? > clazz = null;
        Thread thread = Thread.currentThread();
        ClassLoader classLoader = thread.getContextClassLoader();
        try {
            clazz = classLoader.loadClass(className);
        } catch (ClassNotFoundException e) {
            clazz = Class.forName(className);
        }
        return clazz;
    }
}
