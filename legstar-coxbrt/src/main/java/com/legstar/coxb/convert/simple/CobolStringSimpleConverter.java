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
package com.legstar.coxb.convert.simple;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.convert.ICobolStringConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java
 * strings to cobol character strings.
 * 
 * @author Fady Moussallam
 * 
 */
public class CobolStringSimpleConverter extends CobolSimpleConverter
        implements ICobolStringConverter {

    /** Detects a string content as binary data. */
    private static final Pattern BINARY_CONTENT_PATTERN = Pattern
            .compile("0x[\\da-fA-F]+");

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolStringSimpleConverter(
            final CobolContext cobolContext) {
        super(cobolContext);
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolStringBinding ce,
            final byte[] hostTarget,
            final int offset)
            throws HostException {
        int newOffset = 0;
        try {
            newOffset = toHostSingle(ce.getStringValue(),
                    getCobolContext().getHostCharsetName(),
                    ce.getByteLength(),
                    ce.isJustifiedRight(),
                    hostTarget,
                    offset);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolArrayStringBinding ce,
            final byte[] hostTarget,
            final int offset,
            final int currentOccurs)
            throws HostException {
        int newOffset = offset;
        try {
            for (String javaSource : ce.getStringList()) {
                newOffset = toHostSingle(javaSource,
                        getCobolContext().getHostCharsetName(),
                        ce.getItemByteLength(),
                        ce.isJustifiedRight(),
                        hostTarget,
                        newOffset);
            }
            /* If necessary, fill in the array with missing items */
            for (int i = ce.getStringList().size(); i < currentOccurs; i++) {
                newOffset = toHostSingle("",
                        getCobolContext().getHostCharsetName(),
                        ce.getItemByteLength(),
                        ce.isJustifiedRight(),
                        hostTarget,
                        newOffset);
            }
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int fromHost(
            final ICobolStringBinding ce,
            final byte[] hostSource,
            final int offset)
            throws HostException {
        int newOffset = offset;
        try {
            String javaString = fromHostSingle(
                    getCobolContext().getHostCharsetName(),
                    ce.getByteLength(),
                    hostSource,
                    newOffset);
            ce.setStringValue(javaString);
            newOffset += ce.getByteLength();
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int fromHost(
            final ICobolArrayStringBinding ce,
            final byte[] hostSource,
            final int offset,
            final int currentOccurs)
            throws HostException {
        List < String > lArray = new ArrayList < String >();
        int newOffset = offset;
        try {
            for (int i = 0; i < currentOccurs; i++) {
                String javaString = fromHostSingle(
                        getCobolContext().getHostCharsetName(),
                        ce.getItemByteLength(),
                        hostSource,
                        newOffset);
                lArray.add(javaString);
                newOffset += ce.getItemByteLength();
            }
            ce.setStringList(lArray);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /**
     * Converts a Java String to a host character stream using the host
     * character set unless content represents binary data.
     * <p>
     * Any character string following this pattern: '0x[\\da-fA-F]+' is
     * interpreted as binary content.
     * 
     * @param javaString java string to convert
     * @param hostCharsetName host character set
     * @param cobolByteLength host byte length
     * @param isJustifiedRight is Cobol data right justified
     * @param hostTarget target host buffer
     * @param offset offset in target host buffer
     * @return offset after host buffer is updated
     * @throws CobolConversionException if conversion fails
     */
    public static final int toHostSingle(
            final String javaString,
            final String hostCharsetName,
            final int cobolByteLength,
            final boolean isJustifiedRight,
            final byte[] hostTarget,
            final int offset)
            throws CobolConversionException {

        /* Check that we are still within the host target range */
        int lastOffset = offset + cobolByteLength;
        if (lastOffset > hostTarget.length) {
            throw (new CobolConversionException(
                    "Attempt to write past end of host source buffer",
                    new HostData(hostTarget), offset, cobolByteLength));
        }

        /* HostData before it is positioned in the hostTarget buffer. */
        byte[] hostSource = new byte[0];
        byte padChar;

        /*
         * If data being passed represent binary content convert from
         * hex string otherwise use the host charset for conversion.
         */
        if (isBinaryContent(javaString)) {
            padChar = 0x0;
            hostSource = HostData.toByteArray(javaString.substring(2));
        } else {
            try {
                padChar = " ".getBytes(hostCharsetName)[0];
                if (javaString != null) {
                    hostSource = javaString.getBytes(hostCharsetName);
                }
            } catch (UnsupportedEncodingException e) {
                throw new CobolConversionException(
                        "UnsupportedEncodingException:" + e.getMessage());
            }
        }

        /*
         * The target host element might be larger than the converted java
         * String and might have to be right or left justified.
         */
        int leftPadIndex = offset;
        int rightPadIndex = lastOffset;

        if (hostSource.length < cobolByteLength) {
            if (isJustifiedRight) {
                leftPadIndex = offset + cobolByteLength - hostSource.length;
            } else {
                rightPadIndex = lastOffset - 1 - cobolByteLength
                        + hostSource.length;
            }
        }

        int iSource = 0;
        for (int iTarget = offset; iTarget < lastOffset; iTarget++) {
            if (iTarget < leftPadIndex || iTarget > rightPadIndex) {
                hostTarget[iTarget] = padChar;
            } else {
                hostTarget[iTarget] = hostSource[iSource];
                iSource++;
            }
        }

        return lastOffset;

    }

    /**
     * Check that content represents binary data.
     * 
     * @param javaString the java string which content must be tested
     * @return true if content represents a valid hexadecimal string
     */
    private static boolean isBinaryContent(final String javaString) {
        if (javaString != null
                && javaString.length() > 2
                && javaString.charAt(0) == '0') {
            Matcher matcher = BINARY_CONTENT_PATTERN.matcher(javaString);
            if (matcher.matches()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Converts a host character string into a Java string.
     * 
     * @param hostCharsetName host character set
     * @param cobolByteLength host byte length
     * @param hostSource source host buffer
     * @param offset offset in source host buffer
     * @return offset after host buffer is read
     * @throws CobolConversionException if conversion fails
     */
    public static final String fromHostSingle(
            final String hostCharsetName,
            final int cobolByteLength,
            final byte[] hostSource,
            final int offset)
            throws CobolConversionException {

        String javaString = null;
        int javaStringLength = cobolByteLength;

        /*
         * Check that we are still within the host source range.
         * If not, consider the host optimized its payload by truncating
         * trailing nulls in which case, we just need to process the
         * characters returned if any.
         */
        int lastOffset = offset + cobolByteLength;
        if (lastOffset > hostSource.length) {
            if (offset >= hostSource.length) {
                return javaString;
            } else {
                javaStringLength = hostSource.length - offset;
            }
        }

        /*
         * The Java String is obtained by translating from the host code page
         * to the local code page.
         */
        try {
            /* Trim trailing spaces and low-values unless there is only one */
            byte hostSpace = " ".getBytes(hostCharsetName)[0];
            int i = javaStringLength;
            while (i > 0
                    && (hostSource[offset + i - 1] == hostSpace
                            || hostSource[offset + i - 1] == 0)) {
                i--;
            }
            javaStringLength = i;

            javaString = new String(
                    hostSource, offset, javaStringLength,
                    hostCharsetName);

            /*
             * Some low-value characters may have slipped into the resulting
             * string.
             */
            if (javaString.indexOf("\0") != -1) {
                javaString = javaString.replace('\0', ' ');
            }

            return javaString;
        } catch (UnsupportedEncodingException uee) {
            throw new CobolConversionException(
                    "UnsupportedEncodingException:" + uee.getMessage());
        }

    }

}
