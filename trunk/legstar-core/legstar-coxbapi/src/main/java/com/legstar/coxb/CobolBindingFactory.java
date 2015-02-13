/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

import com.legstar.coxb.util.ClassLoadingException;
import com.legstar.coxb.util.ClassUtil;

/**
 * Instantiate a concrete binding factory.
 */
public final class CobolBindingFactory {

    /**
     * In this version the factory name is hardcoded. In a future release,
     * this will be pulled from some configuration file.
     */
    private static final String FACTORY_NAME =
            "com.legstar.coxb.impl.CBindingFactory";

    /** Private constructor to prevent instantiation of this final class. */
    private CobolBindingFactory() {
    }

    /**
     * Create a concrete binding factory.
     * 
     * @return a binding factory ready to create binding elements
     */
    public static ICobolBindingFactory getBindingFactory() {
        try {
            Object of = ClassUtil.newObject(FACTORY_NAME);
            return (ICobolBindingFactory) of;
        } catch (ClassLoadingException e) {
            throw new RuntimeException(e);
        }
    }

}
