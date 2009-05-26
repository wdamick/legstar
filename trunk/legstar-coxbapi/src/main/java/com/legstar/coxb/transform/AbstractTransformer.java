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

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.CobolBindingVisitorsFactory;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolBindingVisitorsFactory;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.CobolConvertersFactory;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.ICobolConvertersFactory;

/**
 * Transformers provide the foundation for host data transformations.
 * Implementing classes typically provide the binding class and inherit the
 * transformation capabilities.
 */
public abstract class AbstractTransformer implements IHostTransformer {

    /** The current set of COBOL converters.*/
    private ICobolConverters mCobolConverters;
    
    /** Factory that provides concrete implementations of marshalers/unmarshalers. */
    private ICobolBindingVisitorsFactory mCobolBindingVisitorsFactory;
    
    /** Caching the binding allows reuse and better performances.
     * Multiple threads might be using this transformer concurrently so we
     * keep a binding per thread. */
    private ConcurrentMap < Long, ICobolComplexBinding > _cobolComplexBindingCache;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Create a transformer using default COBOL parameters.
     */
    public AbstractTransformer() {
        this(new CobolContext());
    }

    /**
     * Create a transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     */
    public AbstractTransformer(final String hostCharset) {
        this(new CobolContext());
        getCobolContext().setHostCharsetName(hostCharset);
    }

    /**
     * Create a transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public AbstractTransformer(final CobolContext cobolContext) {
        ICobolConvertersFactory factory = CobolConvertersFactory.createCobolConvertersFactory();
        mCobolConverters = factory.createCobolConverters();
        mCobolBindingVisitorsFactory = CobolBindingVisitorsFactory.createCobolBindingVisitorsFactory();
        setCobolContext(cobolContext);
        _cobolComplexBindingCache =
            new ConcurrentHashMap < Long, ICobolComplexBinding >();
    }

    /**
     * This method returns the current set of COBOL converters.
     * @return a set of COBOL converters
     */
    public ICobolConverters getCobolConverters() {
        return mCobolConverters;
    }

    /**
     * Caller can pass his own set of converters if he is not satisfied with the default.
     * @param cobolConverters the new set of COBOL converters
     */
    public void setCobolConverters(final ICobolConverters cobolConverters) {
        mCobolConverters = cobolConverters;
    }

    /**
     * Returns the current COBOL parameter set.
     * @return a COBOL parameter set
     */
    public CobolContext getCobolContext() {
        return getCobolConverters().getCobolContext();
    }

    /**
     * Change the COBOL parameter sets of the converters.
     * @param cobolContext the new COBOL parameter set
     */
    public void setCobolContext(final CobolContext cobolContext) {
        getCobolConverters().setCobolContext(cobolContext);
    }

    /**
     * @return the Factory that provides concrete implementations of marshalers/unmarshalers
     */
    public ICobolBindingVisitorsFactory getCobolBindingVisitorsFactory() {
        return mCobolBindingVisitorsFactory;
    }

    /**
     * @param cobolBindingVisitorsFactory the Factory that provides concrete implementations
     *  of marshalers/unmarshalers to set
     */
    public void setCobolBindingVisitorsFactory(
            final ICobolBindingVisitorsFactory cobolBindingVisitorsFactory) {
        mCobolBindingVisitorsFactory = cobolBindingVisitorsFactory;
    }

    /**
     * @return the cached binding if one is available otherwise will request a 
     *  new one from a descendant.
     * @throws CobolBindingException if binding cannot be built
     */
    public ICobolComplexBinding getCachedBinding() throws CobolBindingException {
        /* Get a cached transformer for the current thread */
        Long id = Long.valueOf(Thread.currentThread().getId());
        ICobolComplexBinding binding = _cobolComplexBindingCache.get(id);
        if (binding == null) {
            if (_log.isDebugEnabled()) {
                _log.debug("Creating new binding for thread: " + id + " object: " + this);
            }
            ICobolComplexBinding newBinding = getBinding();
            binding = _cobolComplexBindingCache.putIfAbsent(id, newBinding);
            if (binding == null) {
                binding = newBinding;
            }
        }
        return binding;
    }

}
