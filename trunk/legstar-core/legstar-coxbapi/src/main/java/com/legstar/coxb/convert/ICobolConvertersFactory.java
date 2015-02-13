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
package com.legstar.coxb.convert;

import com.legstar.coxb.CobolContext;

/**
 * Concrete implementations of the CobolConvertsFactory implement this interface.
 *
 */
public interface ICobolConvertersFactory {
    
    /**
     * Create an instance of CobolConverters which are a set of
     * converters for each COBOL data type.
     * This method uses the default COBOL context.
     * @return an implementation of {@link com.legstar.coxb.convert.ICobolConverters}
     */
    ICobolConverters createCobolConverters();

    /**
     * Create an instance of CobolConverters which are a set of
     * converters for each COBOL data type.
     * @param cobolContext the COBOL parameter set
     * @return an implementation of {@link com.legstar.coxb.convert.ICobolConverters}
     */
    ICobolConverters createCobolConverters(final CobolContext cobolContext);
}
