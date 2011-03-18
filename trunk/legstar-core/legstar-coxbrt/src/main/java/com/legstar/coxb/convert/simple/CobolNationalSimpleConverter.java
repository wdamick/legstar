/*******************************************************************************
 * Copyright (c) 2011 LegSem.
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
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.convert.ICobolNationalConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java 
 * strings to cobol national strings.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolNationalSimpleConverter extends CobolSimpleConverter
implements ICobolNationalConverter {

    /** UTF-16 code point for space character. */
    private static final byte[] SPACE_UTF_16 = {0x00, 0x20}; 

    /** Encoding used for national items. */
    private static final String NATIONAL_CHARSET = "UTF-16BE";

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolNationalSimpleConverter(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolNationalBinding ce,
            final byte[] hostTarget,
            final int offset)
    throws HostException {
        int newOffset = 0;
        try {
            newOffset = toHostSingle(ce.getStringValue(),
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
            final ICobolArrayNationalBinding ce,
            final byte[] hostTarget,
            final int offset,
            final int currentOccurs)
    throws HostException {
        int newOffset = offset;
        try {
            for (String javaSource : ce.getStringList()) {
                newOffset = toHostSingle(javaSource,
                        ce.getItemByteLength(),
                        ce.isJustifiedRight(),
                        hostTarget,
                        newOffset);
            }
            /* If necessary, fill in the array with missing items */
            for (int i = ce.getStringList().size();
            i < currentOccurs; i++) {
                newOffset = toHostSingle("",
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
            final ICobolNationalBinding ce,
            final byte[] hostSource,
            final int offset)
    throws HostException {
        int newOffset = offset;
        try {
            String javaString = fromHostSingle(
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
            final ICobolArrayNationalBinding ce,
            final byte[] hostSource,
            final int offset,
            final int currentOccurs)
    throws HostException {
        List < String > lArray = new ArrayList < String >();
        int newOffset = offset;
        try {
            for (int i = 0; i < currentOccurs; i++) {
                String javaString = fromHostSingle(
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
     *  Converts a Java String to a host national element.
     * 
     * @param javaString java string to convert
     * @param cobolByteLength host byte length
     * @param isJustifiedRight is Cobol data right justified
     * @param hostTarget target host buffer
     * @param offset offset in target host buffer
     * @return offset after host buffer is updated
     * @throws CobolConversionException if conversion fails
     */
    public static final int toHostSingle(
            final String javaString,
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
                    "".getBytes(NATIONAL_CHARSET);
            } else {
                hostSource =
                    javaString.getBytes(NATIONAL_CHARSET);
            }
        } catch (UnsupportedEncodingException uee) {
            throw new CobolConversionException(
                    "UnsupportedEncodingException:" + uee.getMessage());
        }

        int iSource = 0;
        int hsl = hostSource.length;
        /* The target host element might be larger than the converted java
         * String and might have to be right or left justified. The padding
         * code point is 0x0020 (space character). */
        int iTarget = offset;
        boolean flip = true;

        /* Pad with initial spaces if necessary */
        if (isJustifiedRight) {
            while (iTarget < (lastOffset - hsl)) {
                hostTarget[iTarget] = SPACE_UTF_16[(flip) ? 0 : 1];
                iTarget++;
                flip = !flip;
            }
        }

        /* Continue on with source content */
        while (iSource < hostSource.length && iTarget < lastOffset) {
            hostTarget[iTarget] = hostSource[iSource];
            iSource++;
            iTarget++;
        }

        /* Pad with final spaces if necessary */
        flip = true;
        if (!isJustifiedRight) {
            while (iTarget < lastOffset) {
                hostTarget[iTarget] = SPACE_UTF_16[(flip) ? 0 : 1];
                iTarget++;
                flip = !flip;
            }
        }

        return lastOffset;
    }

    /** Converts a host national string into a Java string.
     * 
     * @param cobolByteLength host byte length
     * @param hostSource source host buffer
     * @param offset offset in source host buffer
     * @return offset after host buffer is read
     * @throws CobolConversionException if conversion fails
     */
    public static final String fromHostSingle(
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

        /* The Java String is obtained by translating from the host code page
         * to the local code page. */
        try {
            javaString = new String(
                    hostSource, offset, javaStringLength,
                    NATIONAL_CHARSET);
            /* Some low-value characters may have slipped into the resulting
             * string. */
            if (javaString.indexOf("\0") != -1) {
                javaString = javaString.replace('\0', ' ');
            }
        } catch (UnsupportedEncodingException uee) {
            throw new CobolConversionException(
                    "UnsupportedEncodingException:" + uee.getMessage());
        }

        return javaString;
    }
}
