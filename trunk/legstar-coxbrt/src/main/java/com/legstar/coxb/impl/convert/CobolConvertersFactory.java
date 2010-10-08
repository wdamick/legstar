/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.convert;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.ICobolConvertersFactory;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;

/**
 * Provides a concrete implementation of COBOL converters.
 * TODO Simple converters should be an optional implementation with the
 *  concrete class name coming from a configuration file.
 *
 */
public class CobolConvertersFactory implements ICobolConvertersFactory {

    /** {@inheritDoc} */
    public ICobolConverters createCobolConverters() {
        return new CobolSimpleConverters(new CobolContext());
    }

    /** {@inheritDoc} */
    public ICobolConverters createCobolConverters(final CobolContext cobolContext) {
        return new CobolSimpleConverters(cobolContext);
    }

}
