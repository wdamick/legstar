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

import java.util.ArrayList;
import java.util.List;

/**
 * Utilities that are common to the binding API and dependents.
 * 
 * Part of this code is more or less a clone of Sun's
 *  <code>com.sun.xml.bind.api.impl.NameUtil</code>.
 * It's just that dragging the entire jaxb-impl as a dependency just to get the
 * naming right is overkill.
 * The jaxb-impl dependency is a real pain since it is now included in JRE and
 * brings in all forms of version conflicts.
 *
 */
public final class Utils {
    
    // the 5-category classification that we use in this code
    // to find work breaks
    /** Upper case letters. */
    private static final int UPPER_LETTER = 0;
    /** Lower case letters. */
    private static final int LOWER_LETTER = 1;
    /** Other letters. */
    private static final int OTHER_LETTER = 2;
    /** Digits letters. */
    private static final int DIGIT = 3;
    /** Yet others (Non digit, non letter). */
    private static final int OTHER = 4;

    /**
     * A utility class.
     */
    private Utils() {
        
    }

    /**
     * NOTE: This code is already in com.legstar.util.JaxbUtil. But we dont
     * want to create a dependency on the coxb implementation here.
     * TODO find a way to share this code.
     * Rather than using the Class.forName mechanism, this uses
     * Thread.getContextClassLoader instead. In a Servlet context such as
     * Tomcat, this allows JAXB classes for instance to be loaded from the
     * web application (webapp) location while this code might have been
     * loaded from shared/lib.
     * If Thread.getContextClassLoader fails to locate the class then we
     * give a last chance to Class.forName.
     * @param className the class name to load
     * @return the class
     * @throws ClassNotFoundException if class is not accessible from this
     * thread loader
     */
    public static Class < ? > loadClass(
            final String className) throws ClassNotFoundException {
        Class < ? > clazz = null;
        Thread thread = Thread.currentThread();
        ClassLoader classLoader = thread.getContextClassLoader();
        try {
            clazz = classLoader.loadClass(className);
        } catch (ClassNotFoundException e) {
            clazz = Class.forName(className);
        }
        return clazz;
    }

    /**
     * This utility method is used to suppress the need for @SuppressWarnings
     * when we cast objects to List < ? >.
     * @param <T> the list type
     * @param x the object to cast
     * @return a list object
     */
    @SuppressWarnings("unchecked")
    public static < T > T cast(final Object x) {
        return (T) x;
    }

    /**
     * Look up table for actions.
     * type0*5+type1 would yield the action to be taken.
     */
    private static final byte[] ACTION_TABLE = new byte[5 * 5];

    /** initialize the action table */
    static {
        for (int t0 = 0; t0 < 5; t0++) {
            for (int t1 = 0; t1 < 5; t1++) {
                ACTION_TABLE[t0 * 5 + t1] = decideAction(t0, t1);
            }
        }
    }

    /** Need to check if this is a punctuation. */
    private static final byte ACTION_CHECK_PUNCT = 0;
    /** Need to check if this next character is lower case. */
    private static final byte ACTION_CHECK_C2 = 1;
    /** Process a break. */
    private static final byte ACTION_BREAK = 2;
    /** No break action. */
    private static final byte ACTION_NOBREAK = 3;

    /**
     * Decide the action to be taken given
     * the classification of the preceding character 't0' and
     * the classification of the next character 't1'.
     * @param t0 the previous character class
     * @param t1 the current character class
     * @return what action to perform
     */
    private static byte decideAction(final int t0, final int t1) {
        if (t0 == OTHER && t1 == OTHER) {
            return ACTION_CHECK_PUNCT;
        }
        if (!xor(t0 == DIGIT, t1 == DIGIT)) {
            return ACTION_BREAK;
        }
        if (t0 == LOWER_LETTER && t1 != LOWER_LETTER) {
            return ACTION_BREAK;
        }
        if (!xor(t0 <= OTHER_LETTER, t1 <= OTHER_LETTER)) {
            return ACTION_BREAK;
        }
        if (!xor(t0 == OTHER_LETTER, t1 == OTHER_LETTER)) {
            return ACTION_BREAK;
        }

        if (t0 == UPPER_LETTER && t1 == UPPER_LETTER) {
            return ACTION_CHECK_C2;
        }

        return ACTION_NOBREAK;
    }

    /**
     * XOR function.
     * @param x first operand
     * @param y second operand
     * @return true if both operands are true or both are false,
     *  false otherwise
     */
    private static boolean xor(final boolean x, final boolean y) {
        return (x && y) || (!x && !y);
    }

    /**
     * Determine if character is punctuation.
     * @param c the character
     * @return true if punctuation
     */
    public static boolean isPunct(final char c) {
        return c == '-'
            || c == '.'
                || c == ':'
                    || c == '_'
                        || c == '\u00b7'
                            || c == '\u0387'
                                || c == '\u06dd'
                                    || c == '\u06de';
    }

    /**
     * Determine if character is lowercase.
     * @param c the character to test
     * @return true if lower case
     */
    protected static boolean isLower(final char c) {
        return c >= 'a' && c <= 'z' || Character.isLowerCase(c);
    }

    /**
     * Returns a JAXB compatible variable name.
     * @param s the original string
     * @return a java variable name
     */
    public static String toVariableName(final String s) {
        return toMixedCaseName(toWordList(s), false);
    }

    /**
     * Returns a JAXB compatible class name.
     * @param s the original string
     * @return a java class name
     */
    public static String toClassName(final String s) {
        return toMixedCaseName(toWordList(s), true);
    }

    /**
     * Tokenizes a string into words and capitalizes the first
     * character of each word.
     * @param s the original word
     * @return an array of subwords
     *
     * <p>
     * This method uses a change in character type as a splitter
     * of two words. For example, "abc100ghi" will be splitted into
     * {"Abc", "100","Ghi"}.
     */
    public static List < String > toWordList(final String s) {
        ArrayList < String > ss = new ArrayList < String >();
        int n = s.length();
        for (int i = 0; i < n;) {

            // Skip punctuation
            while (i < n) {
                if (!isPunct(s.charAt(i))) {
                    break;
                }
                i++;
            }
            if (i >= n) {
                break;
            }

            // Find next break and collect word
            int b = nextBreak(s, i);
            String w = (b == -1) ? s.substring(i) : s.substring(i, b);
            ss.add(escape(capitalize(w)));
            if (b == -1) {
                break;
            }
            i = b;
        }

        // we can't guarantee a valid Java identifier anyway,
        // so there's not much point in rejecting things in this way.
        // if (ss.size() == 0)
        // throw new IllegalArgumentException("Zero-length identifier");
        return ss;
    }

    /**
     * Capitalizes the first character of the specified string,
     * and de-capitalize the rest of characters.
     * @param s the original word
     * @return the capitalized word
     */
    public static String capitalize(final String s) {
        if (!isLower(s.charAt(0))) {
            return s;
        }
        StringBuilder sb = new StringBuilder(s.length());
        sb.append(Character.toUpperCase(s.charAt(0)));
        sb.append(s.substring(1).toLowerCase());
        return sb.toString();
    }

    /**
     * Lookup the next break.
     * Precondition: s[start] is not punctuation
     * @param s the original word
     * @param start where to start looking
     * @return position of next break
     */
    private static int nextBreak(final String s, final int start) {
        int n = s.length();

        char c1 = s.charAt(start);
        int t1 = classify(c1);

        for (int i = start + 1; i < n; i++) {
            // shift (c1,t1) into (c0,t0)
            // char c0 = c1;  --- conceptually, but c0 won't be used
            int t0 = t1;

            c1 = s.charAt(i);
            t1 = classify(c1);

            switch(ACTION_TABLE[t0 * 5 + t1]) {
            case ACTION_CHECK_PUNCT:
                if (isPunct(c1)) {
                    return i;
                }
                break;
            case ACTION_CHECK_C2:
                if (i < n - 1) {
                    char c2 = s.charAt(i + 1);
                    if (isLower(c2)) {
                        return i;
                    }
                }
                break;
            case ACTION_BREAK:
                return i;
            default: break;
            }
        }
        return -1;
    }

    /**
     * Concatenates the pieces into a mixed case name.
     * @param ss the array of strings to concatenate
     * @param startUpper leaves casing unchanged of true, otherwise
     * lowercases the first sequence
     * @return the combined mixed cas word
     */
    public static String toMixedCaseName(
            final List < String > ss, final boolean startUpper) {
        StringBuilder sb = new StringBuilder();
        if (!ss.isEmpty()) {
            sb.append(startUpper ? ss.get(0) : ss.get(0).toLowerCase());
            for (int i = 1; i < ss.size(); i++) {
                sb.append(ss.get(i));
            }
        }
        return sb.toString();
    }

    /**
     * Escapes characters that are unusable as Java identifiers
     * by replacing unsafe characters with safe characters.
     * @param s the original word
     * @return the escaped string
     */
    private static String escape(final String s) {
        int n = s.length();
        for (int i = 0; i < n; i++) {
            if (!Character.isJavaIdentifierPart(s.charAt(i))) {
                StringBuilder sb = new StringBuilder(s.substring(0, i));
                escape(sb, s, i);
                return sb.toString();
            }
        }
        return s;
    }

    /**
     * Escapes characters is the given string so that they can be
     * printed by only using US-ASCII characters.
     *
     * The escaped characters will be appended to the given
     * StringBuffer.
     *
     * @param sb
     *      StringBuffer that receives escaped string.
     * @param s
     *      String to be escaped. <code>s.substring(start)</code>
     *      will be escaped and copied to the string buffer.
     * @param start where to start
     */
    private static void escape(
            final StringBuilder sb, final String s, final int start) {
        int n = s.length();
        for (int i = start; i < n; i++) {
            char c = s.charAt(i);
            if (Character.isJavaIdentifierPart(c)) {
                sb.append(c);
            } else {
                sb.append('_');
                if (c <= '\u000f') {
                    sb.append("000");
                } else if (c <= '\u00ff') {
                    sb.append("00");
                } else if (c <= '\u0fff') {
                    sb.append('0');
                }
                sb.append(Integer.toString(c, 16));
            }
        }
    }

    /**
     * Classify a character into 5 categories that determine the word break.
     * @param c0 character to classify
     * @return the character class
     */
    private static  int classify(final char c0) {
        switch(Character.getType(c0)) {
        case Character.UPPERCASE_LETTER:        return UPPER_LETTER;
        case Character.LOWERCASE_LETTER:        return LOWER_LETTER;
        case Character.TITLECASE_LETTER:
        case Character.MODIFIER_LETTER:
        case Character.OTHER_LETTER:            return OTHER_LETTER;
        case Character.DECIMAL_DIGIT_NUMBER:    return DIGIT;
        default:                                return OTHER;
        }
    }
}
