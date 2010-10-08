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

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayDbcsBinding;
import com.legstar.coxb.ICobolDbcsBinding;
import com.legstar.coxb.convert.ICobolDbcsConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java 
 * strings to cobol DBCS (PIC G DISPLAY-1) character strings.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolDbcsSimpleConverter extends CobolSimpleConverter
implements ICobolDbcsConverter {

    /** Character used on mainframes to start a DBCS sequence.*/
    public static final char SHIFT_IN = 0x0e;

    /** Character used on mainframes to stop a DBCS sequence.*/
    public static final char SHIFT_OUT = 0x0f;

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolDbcsSimpleConverter(
            final CobolContext cobolContext) {
        super(cobolContext);
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolDbcsBinding ce,
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
            final ICobolArrayDbcsBinding ce,
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
            for (int i = ce.getStringList().size();
            i < currentOccurs; i++) {
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
            final ICobolDbcsBinding ce,
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
            final ICobolArrayDbcsBinding ce,
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
     *  Converts a Java String to a host character stream within the host
     *  character set.
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

        /* HostData is obtained by converting the java String content to the 
         * target host character set. */
        byte[] hostSource;

        /* See how many host bytes would be needed to hold the converted
         * string */
        try {
            if (javaString == null) {
                hostSource =
                    "".getBytes(hostCharsetName);
            } else {
                hostSource =
                    javaString.getBytes(hostCharsetName);
            }
            
            /* The target host element might be larger than the converted java
             * String and might have to be right or left justified. The padding
             * character is a space character. */
            int iSource = 0;
            int iTarget = offset;
            int iLength = hostSource.length;
            
            if (iLength > 0) {

                /* The java conversion always adds shift-in/shift-out delimiters
                 * which are not needed for pure DBCS fields. */
                if (hostSource[0] == SHIFT_IN) {
                    iSource++;
                }
                if (hostSource[hostSource.length - 1] == SHIFT_OUT) {
                    iLength--;
                }
            }

            /* Pad with initial spaces if necessary */
            if (isJustifiedRight) {
                iTarget += pad(hostTarget,
                        iTarget, (lastOffset - iLength),
                        hostCharsetName);
            }

            /* Continue on with source content */
            while (iSource < iLength && iTarget < lastOffset) {
                hostTarget[iTarget] = hostSource[iSource];
                iSource++;
                iTarget++;
            }

            /* Pad with final spaces if necessary */
            if (!isJustifiedRight) {
                iTarget += pad(hostTarget,
                        iTarget, lastOffset,
                        hostCharsetName);
            }
            return lastOffset;
        } catch (UnsupportedEncodingException uee) {
            throw new CobolConversionException(
                    "UnsupportedEncodingException:" + uee.getMessage());
        }

    }

    /** Converts a host character string into a Java string.
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

        /* Check that we are still within the host source range.
         * If not, consider the host optimized its payload by truncating
         * trailing nulls in which case, we just need to process the
         * characters returned if any. */
        int lastOffset = offset + cobolByteLength;
        if (lastOffset > hostSource.length) {
            if (offset >= hostSource.length) {
                return javaString;
            } else {
                javaStringLength = hostSource.length - offset;
            }
        }
        
        /* The host is not expected to wrap string with shift-in/shift-out
         * while java expects that. We need to append those. */
        byte[] shiftHostSource = new byte[javaStringLength + 2];
        shiftHostSource[0] = SHIFT_IN;
        System.arraycopy(hostSource, offset, shiftHostSource, 1, javaStringLength);
        shiftHostSource[shiftHostSource.length - 1] = SHIFT_OUT;
        javaStringLength += 2;

        /* The Java String is obtained by translating from the host code page
         * to the local code page. */
        try {
            javaString = new String(
                    shiftHostSource, 0, javaStringLength,
                    hostCharsetName);
            /* Some low-value characters may have slipped into the resulting
             * string. */
            if (javaString.indexOf("\0") != -1) {
                javaString = javaString.replace('\0', ' ');
            }
        } catch (UnsupportedEncodingException uee) {
            throw new CobolConversionException(
                    "UnsupportedEncodingException:" + uee.getMessage());
        }

        return javaString.trim();
    }

    /**
     * Determines the padding character depending on the target character set.
     * @param hostCharsetName host character set
     * @return the padding character
     * @throws UnsupportedEncodingException if padding character cannot be
     *  translated
     */
    private static byte getPadChar(
            final String hostCharsetName) throws UnsupportedEncodingException {
        return " ".getBytes(hostCharsetName)[0];
    }

    /**
     * Pads a byte array with the padding character corresponding to the target
     * host character set.
     * <p/>
     * Padding begins at the specified beginIndex and extends to the character
     *  at index endIndex - 1
     * @param bytes the byte array to pad
     * @param beginIndex what index to start from
     * @param endIndex index of first character that is not to be padded
     * @param hostCharsetName host character set
     * @return the number of padding characters used
     * @throws UnsupportedEncodingException if padding character cannot be
     *  translated
     */
    public static int pad(
            final byte[] bytes,
            final int beginIndex,
            final int endIndex,
            final String hostCharsetName) throws UnsupportedEncodingException {
        byte padChar = getPadChar(hostCharsetName);
        int j = 0;
        for (int i = beginIndex; i < endIndex; i++) {
            bytes[i] = padChar;
            j++;
        }
        return j;
    }
}
