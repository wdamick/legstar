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

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;

/**
 * An implementation of the abstract class under test.
 * This uses reflection binding.
 *
 */
public class HostToJavaDplarchtTransformer extends AbstractHostToJavaTransformer {


    /**
     * No-Arg constructor.
     */
    public HostToJavaDplarchtTransformer() {
        super();
    }

    /**
     * Create a Host to Java transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public HostToJavaDplarchtTransformer(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /**
     * Create a Host to Java transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     */
    public HostToJavaDplarchtTransformer(final String hostCharset) {
        super(hostCharset);
    }

    /** {@inheritDoc} */
    public ICobolComplexBinding getBinding() throws CobolBindingException {
        try {
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    new com.legstar.test.coxb.dplarcht.ObjectFactory(), Dfhcommarea.class);
            return ccem;
        } catch (ReflectBindingException e) {
            throw new CobolBindingException(e);
        }
    }

}
