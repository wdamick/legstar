/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;

/**
 * This class implements the behavior of a zoned decimal cobol element bound to
 * a JAXB BigDecimal property.
 *
 * @author Fady Moussallam
 * 
 */
public class CZonedDecimalBinding
extends CNumericBinding
implements ICobolZonedDecimalBinding {

    /**
     * Constructor for a cobol element to java binding.
     * 
     * @param name the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     */
    public CZonedDecimalBinding(
            final String name,
            final String jaxbName,
            final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        super(name, jaxbName, jaxbType, cobolAnnotations, parentBinding);
    }

    /** {@inheritDoc} */
    public final void accept(final CobolElementVisitor cev)
    throws HostException {
        cev.visit(this);
    }

}
