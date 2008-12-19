package com.legstar.coxb.impl.visitor;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBindingVisitorsFactory;
import com.legstar.coxb.convert.ICobolConverters;

/**
 * Provides a concrete implementation of Marshalers/Unmarshalers.
 */
public class CobolBindingVisitorsFactory implements ICobolBindingVisitorsFactory {

    /** {@inheritDoc} */
    public CobolElementVisitor createMarshalVisitor(final byte[] hostBytes,
            final int offset, final ICobolConverters cobolConverters) {
        return new CobolMarshalVisitor(hostBytes, offset, cobolConverters);
    }

    /** {@inheritDoc} */
    public CobolElementVisitor createUnmarshalVisitor(final byte[] hostBytes,
            final int offset, final ICobolConverters cobolConverters) {
        return new CobolUnmarshalVisitor(hostBytes, offset, cobolConverters);
    }

}
