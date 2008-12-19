package com.legstar.coxb;

import com.legstar.coxb.convert.ICobolConverters;

/**
 * Concrete implementations of the CobolBindingVisitorsFactory implement this interface.
 *
 */
public interface ICobolBindingVisitorsFactory {

    /**
     * Create an instance of a Marshal visitor which turns a Java data object to
     * mainframe data as a byte array.
     * @param hostBytes host buffer used by visitor
     * @param offset offset in host buffer
     * @param cobolConverters set of converters to use for cobol elements
     * @return an implementation of {@link com.legstar.coxb.CobolElementVisitor}
     */
    CobolElementVisitor createMarshalVisitor(final byte[] hostBytes,
            final int offset,
            final ICobolConverters cobolConverters);

    /**
     * Create an instance of a Unmarshal visitor which turns mainframe data to a
     * Java data object.
     * @param hostBytes host buffer used by visitor
     * @param offset offset in host buffer
     * @param cobolConverters set of converters to use for cobol elements
     * @return an implementation of {@link com.legstar.coxb.CobolElementVisitor}
     */
    CobolElementVisitor createUnmarshalVisitor(final byte[] hostBytes,
            final int offset,
            final ICobolConverters cobolConverters);
}
