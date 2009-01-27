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
