/*******************************************************************************
 * Copyright (c) 2010 LegSem.
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
import com.legstar.coxb.CobolType;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.util.PictureUtil;

/**
 * This class implements the behavior of a string cobol element bound to
 * a JAXB String property.
 * Numerous COBOL data types map to a String, namely:
 * - Alphabetic PIC A(n)
 * - Alphanumeric PIC X(n)
 * - Alphanumeric edited PIC X/A
 * - External floating point
 * - Numeric edited
 *
 * @author Fady Moussallam
 * 
 */
public class CStringBinding extends AbstractAlphaNumericBinding
implements ICobolStringBinding {

    /**
     * Constructor for a cobol element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     */
    public CStringBinding(
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
        return calcStringByteLength(getPicture(), getCobolType());
    }

    /**
     * Calculates the host byte length for a variety of COBOL data types.
     * @param picture the picture clause
     * @param cobolType the original COBOL data type
     * @return the host byte length
     */
    public static int calcStringByteLength(
            final String picture, final CobolType cobolType) {
        switch (cobolType) {
        case ALPHABETIC_ITEM:
            return PictureUtil.getSymbolsNumber(
                    new char[] {'A'}, picture);
        case ALPHANUMERIC_ITEM:
            return PictureUtil.getSymbolsNumber(
                    new char[] {'A', 'X', '9'}, picture);
        case ALPHANUMERIC_EDITED_ITEM:
            return PictureUtil.getSymbolsNumber(
                    new char[] {'A', 'X', '9', 'B', '0', '/'}, picture);
        case EXTERNAL_FLOATING_ITEM:
            return PictureUtil.getSymbolsNumber(
                    new char[] {'+', '-', '9', '.', 'E'}, picture);
        case NUMERIC_EDITED_ITEM:
            /* TODO the currency sign should not be hardcoded */
            int count = PictureUtil.getSymbolsNumber(
                    new char[] {'B', 'Z', '9', '0', ',', '.', '-', '+', '/', '*', '$'},
                    picture);
            /* In addition, there might be a CR or DB */
            if (picture.indexOf("CR") > -1) {
                count += 2;
            }
            if (picture.indexOf("DB") > -1) {
                count += 2;
            }
            return count;
        default:
            return PictureUtil.getSymbolsNumber(
                    new char[] {'X'}, picture);
        }
    }

}
