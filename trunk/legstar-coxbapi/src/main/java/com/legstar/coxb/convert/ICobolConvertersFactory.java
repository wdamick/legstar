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
