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
package com.legstar.coxb.impl;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolDbcsBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.util.PictureUtil;

/**
 * This class implements the behavior of a PIC G DISPLAY-1 cobol element bound to
 * a JAXB String property.
 *
 * @author Fady Moussallam
 * 
 */
public class CDbcsBinding extends AbstractAlphaNumericBinding
implements ICobolDbcsBinding {

    /**
     * Constructor for a cobol element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     */
    public CDbcsBinding(
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
        return calcDbcsByteLength(getPicture());
    }

    /**
     * Calculates the host byte length for a PIC G(n).
     * @param picture the picture clause
     * @return the host byte length for a PIC G(n)
     */
    public static int calcDbcsByteLength(final String picture) {
        return 2 * PictureUtil.getSymbolsNumber('G', picture);
    }

}
