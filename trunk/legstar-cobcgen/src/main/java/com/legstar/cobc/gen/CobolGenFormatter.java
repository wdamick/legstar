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
package com.legstar.cobc.gen;

import com.legstar.coxb.ICobolBinding;

/**
 * Cobol clause formatting.
 */
public final class CobolGenFormatter {

    /** Cobol picture clause. */
    private static final String PIC_CLAUSE = "PIC %1$s";
    /** Cobol usage clause. */
    private static final String USAGE_CLAUSE = "%1$s";
    /** Cobol default display usaqge. */
    private static final String USAGE_DISPLAY = "DISPLAY";
    /** Cobol justified right clause. */
    private static final String JUST_RIGHT_CLAUSE = "JUST";
    /** Cobol sign leading clause. */
    private static final String SIGN_LEADING_CLAUSE = "LEADING";
    /** Cobol sign separate clause. */
    private static final String SIGN_SEPARATE_CLAUSE = "SEPARATE";
    /** Cobol fixed size array clause. */
    private static final String FIXED_ARRAY_CLAUSE = "OCCURS %1$d";
    /** Cobol variable size array clause. */
    private static final String VARIABLE_ARRAY_CLAUSE =
        "TO %1$d DEPENDING ON %2$s";
    /** Cobol redefines clause. */
    private static final String REDEFINES_CLAUSE = "REDEFINES %1$s";
    /** Cobol value clause. */
    private static final String VALUE_CLAUSE = "VALUE %1$s";

    /**
     * Private constructor to stop anyone from instantiating
     * this class - the static methods should be used
     * explicitly.
     */
    private CobolGenFormatter()  {
    }

    /**
     * Generate a cobol sentence for a binding item.
     * @param cb the cobol binding item
     * @param indentFactor the indentation factor
     * @return the cobol sentence
     */
    public static String formatCobolClause(
            final ICobolBinding cb,
            final int indentFactor) {
        CobolGenSentence s = new CobolGenSentence(indentFactor);
        s.addClause(String.format("%1$02d", cb.getLevelNumber()));
        s.addClause(cb.getCobolName());
        if (cb.getPicture() != null && cb.getPicture().length() > 0) {
            s.addClause(String.format(PIC_CLAUSE, cb.getPicture()));
        }
        if (cb.getUsage() != null
                && cb.getUsage().length() > 0
                && cb.getUsage().compareToIgnoreCase(USAGE_DISPLAY) != 0) {
            s.addClause(String.format(USAGE_CLAUSE, cb.getUsage()));
        }
        if (cb.isJustifiedRight()) {
            s.addClause(JUST_RIGHT_CLAUSE);
        }
        if (cb.isSignLeading()) {
            s.addClause(SIGN_LEADING_CLAUSE);
        }
        if (cb.isSignSeparate()) {
            s.addClause(SIGN_SEPARATE_CLAUSE);
        }
        if (cb.getMaxOccurs() > 0) {
            if (cb.getMaxOccurs() == cb.getMinOccurs()) {
                s.addClause(String.format(FIXED_ARRAY_CLAUSE,
                        cb.getMaxOccurs()));
            } else {
                s.addClause(String.format(FIXED_ARRAY_CLAUSE,
                        cb.getMinOccurs()));
                s.addClause(String.format(VARIABLE_ARRAY_CLAUSE,
                        cb.getMaxOccurs(),
                        cb.getDependingOn()));
            }
        }
        if (cb.getRedefines() != null && cb.getRedefines().length() > 0) {
            s.addClause(String.format(REDEFINES_CLAUSE, cb.getRedefines()));
        }
        if (cb.getDefaultValue() != null && cb.getDefaultValue().length() > 0) {
            s.addClause(String.format(VALUE_CLAUSE, cb.getDefaultValue()));
        }
        s.close();
        return s.toString();
    }


}
