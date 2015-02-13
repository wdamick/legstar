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
package com.legstar.coxb.impl;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;

/**
 * This class implements the behavior of a packed decimal cobol element bound to
 * a JAXB BigDecimal property.
 *
 * @author Fady Moussallam
 * 
 */
public class CPackedDecimalBinding
extends AbstractNumericBinding
implements ICobolPackedDecimalBinding {

    /**
     * Constructor for a cobol element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     */
    public CPackedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
    }

    /** {@inheritDoc} */
    public void accept(final CobolElementVisitor cev)
    throws HostException {
        cev.visit(this);
    }

    /** {@inheritDoc} */
    public int calcByteLength() {
        return calcPackedDecimalByteLength(getTotalDigits());
    }

    /**
     * Calculates the host byte length for a COMP-3.
     * Every couple of digits is encoded in one byte. The sign is encoded
     * in the last half byte.
     * @param totalDigits the number of digits (including fraction digits)
     * @return the host byte length for a COMP-3
     */
    public static int calcPackedDecimalByteLength(final int totalDigits) {
        return (totalDigits / 2) + 1;
    }
}
