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
package com.legstar.coxb.util;

import java.util.Locale;

/**
 * These are methods to help analyze a BOL picture clause.
 *
 */
public final class PictureUtil {
    
    /** Utility class. */
    private PictureUtil() {
        
    }
    
    /**
     * In order to simply parsing of picture clauses, this gets rid of
     * any white space (which is illegal in picture clause anyway) and
     * turns the sequence to uppercase.
     * @param input the picture clause before
     * @return the picture clause after
     */
    public static String preparePicture(final String input) {
        return input.replace(" ", "").toUpperCase(Locale.getDefault());
    }
    
    /**
     * Examines the input for the requested symbol (case insensitive) and
     * returns the number of symbols found. Symbols can appear individually (XXX)
     * or have multipliers between parentheses (X(nn)).
     * @param symbol the symbol to look up
     * @param input the character sequence
     * @return the number of symbols found
     */
    public static int getSymbolsNumber(final char symbol, final String input) {
        return getSymbolsNumber(new char[] {symbol}, input);
    }
    
    /**
     * Examines the input for the requested symbols (case insensitive) and
     * returns the number of symbols found. Symbols can appear individually (XXX)
     * or have multipliers between parentheses (X(nn)).
     * @param symbols the collection of symbols to look up
     * @param input the character sequence
     * @return the number of symbols found
     */
    public static int getSymbolsNumber(final char[] symbols, final String input) {
        int num = 0;
        for (int i = 0; i < input.length(); i++) {
            for (int j = 0; j < symbols.length; j++) {
                if (input.charAt(i) == symbols[j]) {
                    if ((i < input.length() - 1) && (input.charAt(i + 1) == '(')) {
                        int k = input.indexOf(')', i);
                        num += getNumericInParentheses(input.substring(i, k));
                        i = k;
                    } else {
                        num++;
                    }
                    break;
                }
            }
        }
        return num;
    }

    /**
     * For a character sequence such as (nnn), this will extract the
     * numeric value between parentheses.
     * @param input starts with open parentheses
     * @return the numeric value between parentheses
     */
    public static int getNumericInParentheses(final String input) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < input.length(); i++) {
            if (Character.isDigit(input.charAt(i))) {
                sb.append(input.charAt(i));
            }
        }
        return Integer.parseInt(sb.toString());
    }

}
