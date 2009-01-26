/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.jaxb.plugin;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import junit.framework.TestCase;

/**
 * Testing enumerations.
 *
 */
public class EnumCases extends TestCase {

    /** Target location for generated JAXB classes. */
    private static final File GEN_SRC_DIR = new File("target/src/gen/java");

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(EnumCases.class);

    /**
     * Check Get Set methods on enumerations.
     * @throws Exception if test fails
     */
    public void testGetSetEnum() throws Exception {
        /* Load the generated class */
        URI loc = GEN_SRC_DIR.toURI();
        URL[] ua = {loc.toURL()};
        URLClassLoader cl = new URLClassLoader(ua);
        Class < ? > clazzSearchRequestType = cl.loadClass("com.legstar.test.coxb.enumvar.SearchRequestType");
        Object searchRequest = clazzSearchRequestType.newInstance();

        /* Create an Enum type with a value */
        Class < ? > clazzSafeSearchOptionsType = cl.loadClass("com.legstar.test.coxb.enumvar.SafeSearchOptionsType");
        Field[] fields = clazzSafeSearchOptionsType.getDeclaredFields();
        for (Field field : fields) {
            LOG.debug(field.getName());
        }

        /* Create a new enum type with a value */
        Method getValueMethod = clazzSafeSearchOptionsType.getMethod("value", (Class[]) null);
        Method fromValueMethod = clazzSafeSearchOptionsType.getMethod("fromValue",
                new Class[] {getValueMethod.getReturnType()});
        Object safeSearchOption = fromValueMethod.invoke(null, new Object [] { "Strict" });

        /* Get the value of an Enum */
        getValueMethod.invoke(safeSearchOption, (Object[]) null);

        /* Set the Enum value*/
        Class < ? > [] param = {clazzSafeSearchOptionsType};
        String setterName = "setSafeSearch";
        Method setter =
            searchRequest.getClass().getMethod(setterName, param);
        setter.invoke(searchRequest, safeSearchOption);

        String getterName = "getSafeSearch";
        Method getter = searchRequest.getClass().getMethod(getterName);
        Object result = getter.invoke(searchRequest);
        assertEquals("Strict", getValueMethod.invoke(result, (Object[]) null));
    }


}
