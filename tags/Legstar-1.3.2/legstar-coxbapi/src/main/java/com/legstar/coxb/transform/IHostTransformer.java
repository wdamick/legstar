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
package com.legstar.coxb.transform;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolBindingVisitorsFactory;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.ICobolConverters;

/**
 * Transformers convert complex types to/from raw mainframe byte arrays described
 * by COBOL structures.
 */
public interface IHostTransformer {

    /**
     * This method returns the current set of COBOL converters.
     * @return a set of COBOL converters
     */
    ICobolConverters getCobolConverters();

    /**
     * Caller can pass his own set of converters if he is not satisfied with the default.
     * @param cobolConverters the new set of COBOL converters
     */
    void setCobolConverters(final ICobolConverters cobolConverters);

    /**
     * Returns the current COBOL parameter set.
     * @return a COBOL parameter set
     */
    CobolContext getCobolContext();

    /**
     * Change the COBOL parameter sets of the converters.
     * @param cobolContext the new COBOL parameter set
     */
    void setCobolContext(final CobolContext cobolContext);

    /**
     * @return the Factory that provides concrete implementations of marshalers/unmarshalers
     */
    ICobolBindingVisitorsFactory getCobolBindingVisitorsFactory();

    /**
     * @param cobolBindingVisitorsFactory the Factory that provides concrete implementations
     *  of marshalers/unmarshalers to set
     */
    void setCobolBindingVisitorsFactory(
            final ICobolBindingVisitorsFactory cobolBindingVisitorsFactory);

    /**
     * @return the binding corresponding to the host structure type.
     * Such a binding can either be statically produced by {@link com.legstar.coxb.gen.CoxbBindingGenerator},
     * or dynamically built by {@link com.legstar.coxb.impl.reflect.CComplexBinding}.
     * @throws CobolBindingException if binding cannot be returned
     */
    ICobolComplexBinding getBinding() throws CobolBindingException;

}
