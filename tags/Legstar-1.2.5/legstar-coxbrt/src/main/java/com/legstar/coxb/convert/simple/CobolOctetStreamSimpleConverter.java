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
package com.legstar.coxb.convert.simple;

import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.convert.ICobolOctetStreamConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java
 * byte arrays to cobol octet streams.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolOctetStreamSimpleConverter extends CobolSimpleConverter
implements ICobolOctetStreamConverter {

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolOctetStreamSimpleConverter(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolOctetStreamBinding ce,
            final byte[] hostTarget,
            final int offset)
    throws HostException {
        int newOffset = 0;
        try {
            newOffset = toHostSingle(ce.getByteArrayValue(),
                    ce.getByteLength(),
                    hostTarget,
                    offset);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolArrayOctetStreamBinding ce,
            final byte[] hostTarget,
            final int offset,
            final int currentOccurs)
    throws HostException {
        int newOffset = offset;
        try {
            for (byte[] javaSource : ce.getByteArrayList()) {
                newOffset = toHostSingle(javaSource,
                        ce.getItemByteLength(),
                        hostTarget,
                        newOffset);
            }
            /* If necessary, fill in the array with missing items */
            for (int i = ce.getByteArrayList().size();
            i < currentOccurs; i++) {
                newOffset = toHostSingle(new byte[] { 0 },
                        ce.getItemByteLength(),
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
            final ICobolOctetStreamBinding ce,
            final byte[] hostSource,
            final int offset)
    throws HostException {
        int newOffset = offset;
        try {
            byte[] javaBytes = fromHostSingle(ce.getByteLength(),
                    hostSource,
                    newOffset);
            ce.setByteArrayValue(javaBytes);
            newOffset += ce.getByteLength();
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int fromHost(
            final ICobolArrayOctetStreamBinding ce,
            final byte[] hostSource,
            final int offset,
            final int currentOccurs)
    throws HostException {
        List < byte[] > lArray = new ArrayList < byte[] >();
        int newOffset = offset;
        try {
            for (int i = 0; i < currentOccurs; i++) {
                byte[] javaBytes = fromHostSingle(ce.getItemByteLength(),
                        hostSource,
                        newOffset);
                lArray.add(javaBytes);
                newOffset += ce.getItemByteLength();
            }
            ce.setByteArrayList(lArray);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /**
     *  Converts a Java byte array to a host octet stream.
     * 
     * @param javaBytes java byte array to convert
     * @param cobolByteLength host byte length
     * @param hostTarget target host buffer
     * @param offset offset in target host buffer
     * @return offset after host buffer is updated
     * @throws CobolConversionException if conversion fails
     */
    public static final int toHostSingle(
            final byte[] javaBytes,
            final int cobolByteLength,
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

        /* Provide a default if input is null */
        byte[] localBytes = javaBytes;
        if (localBytes == null) {
            localBytes = new byte[cobolByteLength];
        }
        /**
         * HostData is obtained by moving the java byte array content to the
         * target host buffer. If there are more java bytes than space in the
         * target buffer, data is truncated.
         */
        for (int i = 0; i < localBytes.length && i < cobolByteLength; i++) {
            hostTarget[offset + i] = localBytes[i];
        }
        return lastOffset;
    }

    /** Converts a host octet stream to a java byte array.
     * 
     * @param cobolByteLength host byte length
     * @param hostSource source host buffer
     * @param offset offset in source host buffer
     * @return offset after host buffer is read
     * @throws CobolConversionException if conversion fails
     */
    public static final byte[] fromHostSingle(
            final int cobolByteLength,
            final byte[] hostSource,
            final int offset) throws CobolConversionException {

        /* Check that we are still within the host source range.
         * If not, consider the host optimized its payload by truncating
         * trailing nulls in which case, we just need to recover the
         * partial data returned and then fill with nulls. */
        int lastOffset = offset + cobolByteLength;
        int fill = 0;
        if (lastOffset > hostSource.length) {
            if (offset >= hostSource.length) {
                fill = cobolByteLength;
            } else {
                fill = hostSource.length - offset;
            }
        }

        /* The Java byte array is the exact byte by byte copy of the host octet
         * stream. */
        byte[] javaBytes = new byte[cobolByteLength];
        for (int i = 0; i < cobolByteLength - fill; i++) {
            javaBytes[i] = hostSource[offset + i];
        }
        for (int i = cobolByteLength - fill; i < cobolByteLength; i++) {
            javaBytes[i] = 0x00;
        }

        return javaBytes;
    }
}
