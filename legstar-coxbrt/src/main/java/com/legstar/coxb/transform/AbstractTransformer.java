package com.legstar.coxb.transform;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.BindingException;

/**
 * Transformers provide the foundation for host data transformations.
 * Implementing classes typically provide the binding class and inherit the
 * transformation capabilities.
 */
public abstract class AbstractTransformer {

    /** The current set of COBOL converters.*/
    private ICobolConverters mCobolConverters;
    /**
     * @return the binding corresponding to the host structure type.
     * Such a binding can either be statically produced by {@link com.legstar.coxb.gen.CoxbBindingGenerator},
     * or dynamically built by {@link com.legstar.coxb.impl.reflect.CComplexBinding}.
     * @throws BindingException if binding cannot be returned
     */
    public abstract ICobolComplexBinding getBinding() throws BindingException;

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
        CobolContext cobolContext = new CobolContext();
        cobolContext.setHostCharsetName(hostCharset);
        setCobolContext(cobolContext);
    }

    /**
     * Create a transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public AbstractTransformer(final CobolContext cobolContext) {
        setCobolContext(cobolContext);
    }

    /**
     * This method returns the current set of COBOL converters if any. If none was
     * created yet, this method attempts to create a new one.
     * TODO get the converters from a Factory
     * @return a set of COBOL converters
     */
    public ICobolConverters getCobolConverters() {
        if (mCobolConverters == null) {
            mCobolConverters = new CobolSimpleConverters(new CobolContext());
        }
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

}
