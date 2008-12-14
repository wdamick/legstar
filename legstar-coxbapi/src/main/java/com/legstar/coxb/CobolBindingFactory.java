/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

/**
 * Instanciate a concrete binding factory.
 */
public final class CobolBindingFactory {

    /** In this version the factory name is hardcoded. In a future release,
     * this will be pulled from some configuration file. */
    private static final String FACTORY_NAME =
        "com.legstar.coxb.impl.CBindingFactory";

    /** Private constructor to prevent instantiation of this final class. */
    private CobolBindingFactory() {
    }

    /**
     * Create a concrete binding factory.
     * @return a binding factory ready to create binding elements
     */
    public static ICobolBindingFactory getBindingFactory() {
        try {
            Class < ? > ofClass = loadClass(FACTORY_NAME);
            Object of = ofClass.newInstance();
            return (ICobolBindingFactory) of;
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
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
