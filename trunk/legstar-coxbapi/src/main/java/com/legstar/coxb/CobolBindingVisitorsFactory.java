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
package com.legstar.coxb;

import com.legstar.coxb.util.Utils;

/**
 * Concrete visitors factories are hidden behind this factory. This allow the
 * API to be independent from concrete implementations of marshaling/unmarshaling visitors.
 *
 */
public class CobolBindingVisitorsFactory {

    /** In this version the factory name is hardcoded. In a future release,
     * this will be pulled from some configuration file. */
    private static final String FACTORY_NAME =
        "com.legstar.coxb.impl.visitor.CobolBindingVisitorsFactory";

    /**
     * It is a utility class. Prevent instantiation.
     */
    private CobolBindingVisitorsFactory() {
        
    }
    
    /**
     * @return an implementation of a concrete factory.
     */
    public static ICobolBindingVisitorsFactory createCobolBindingVisitorsFactory() {
        try {
            Class < ? > ofClass = Utils.loadClass(FACTORY_NAME);
            Object of = ofClass.newInstance();
            return (ICobolBindingVisitorsFactory) of;
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

}
