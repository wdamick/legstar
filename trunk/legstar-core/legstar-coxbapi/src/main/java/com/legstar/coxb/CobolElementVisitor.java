/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

import java.util.Hashtable;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.host.HostException;

/**
 * This class is a super-class of all visitors (visitor pattern) involved in
 * converting java object trees instances to host data buffers.
 * 
 * @author Fady Moussallam
 * 
 */
public abstract class CobolElementVisitor {

    /** The current host buffer used by visitor. */
    private byte[] mHostBytes;

    /** The current offset in host buffer. */
    private int mOffset;

    /** The set of converters to use for cobol elements. */
    private ICobolConverters mCobolConverters;

    /** Children elements marked as custom variables. */
    private Hashtable < String, Object > mVariablesMap;

    /**
     * When this is not zero, it means the last COBOL item did not consume all
     * the bytes that it should have and yet the next item is not variably
     * located. In such a case we add a virtual filler length to the offset
     * before we start processing the next item.
     */
    private int _virtualFillerLength = 0;

    /** Logger. */
    private final Log _log = LogFactory.getLog(CobolElementVisitor.class);

    /**
     * No-arg constructor.
     */
    public CobolElementVisitor() {
        mHostBytes = null;
        mOffset = 0;
        mCobolConverters = null;
        mVariablesMap = new Hashtable < String, Object >();
    }

    /**
     * Constructor for a given host buffer and converters set.
     * 
     * @param hostBytes host buffer used by visitor
     * @param offset offset in host buffer
     * @param cobolConverters set of converters to use for cobol elements
     */
    public CobolElementVisitor(final byte[] hostBytes, final int offset,
            final ICobolConverters cobolConverters) {
        mHostBytes = hostBytes;
        mOffset = offset;
        mCobolConverters = cobolConverters;
        mVariablesMap = new Hashtable < String, Object >();
    }

    /**
     * Visit method of visitor pattern for complex elements.
     * 
     * @param ce complex element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolComplexBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for choice elements.
     * 
     * @param ce choice element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolChoiceBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for arrays of complex elements.
     * 
     * @param ce complex array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayComplexBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for single Strings.
     * 
     * @param ce String element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolStringBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for String arrays.
     * 
     * @param ce String array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayStringBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for single Nationals.
     * 
     * @param ce National element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolNationalBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for National arrays.
     * 
     * @param ce National array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayNationalBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for single Dbcs.
     * 
     * @param ce Dbcs element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolDbcsBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for Dbcs arrays.
     * 
     * @param ce Dbcs array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayDbcsBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for single zoned decimals.
     * 
     * @param ce Zoned decimal element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolZonedDecimalBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for zoned decimal arrays.
     * 
     * @param ce Zoned decimal array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayZonedDecimalBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for single packed decimals.
     * 
     * @param ce Packed decimal element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolPackedDecimalBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for packed decimal arrays.
     * 
     * @param ce Packed dcimal array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayPackedDecimalBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for single Binary elements.
     * 
     * @param ce Binary element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolBinaryBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for Binary arrays.
     * 
     * @param ce Binary array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayBinaryBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for single Float elements.
     * 
     * @param ce Float element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolFloatBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for Float arrays.
     * 
     * @param ce Float array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayFloatBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for single Double elements.
     * 
     * @param ce Double element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolDoubleBinding ce) throws HostException;

    /**
     * Visit method of visitor pattern for Double arrays.
     * 
     * @param ce Double array element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayDoubleBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for single Octet streams.
     * 
     * @param ce Octet stream element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolOctetStreamBinding ce)
            throws HostException;

    /**
     * Visit method of visitor pattern for Octet stream arrays.
     * 
     * @param ce Octet stream element descriptor
     * @throws HostException error while visiting
     */
    public abstract void visit(ICobolArrayOctetStreamBinding ce)
            throws HostException;

    /**
     * @return Returns the CobolConverters.
     */
    public ICobolConverters getCobolConverters() {
        return mCobolConverters;
    }

    /**
     * @param cobolConverters The CobolConverters to set.
     */
    public void setCobolConverters(final ICobolConverters cobolConverters) {
        mCobolConverters = cobolConverters;
    }

    /**
     * @return Returns the mHostBytes.
     */
    public byte[] getHostBytes() {
        return mHostBytes;
    }

    /**
     * @param hostBytes The mHostBytes to set.
     */
    public void setHostBytes(final byte[] hostBytes) {
        mHostBytes = hostBytes;
    }

    /**
     * @return Returns the mOffset.
     */
    public int getOffset() {
        return mOffset;
    }

    /**
     * @param offset The mOffset to set.
     */
    public void setOffset(final int offset) {
        mOffset = offset;
    }

    /**
     * @return the current variables map.
     */
    public Hashtable < String, Object > getVariablesMap() {
        return mVariablesMap;
    }

    /**
     * @param variablesMap the variables map to set
     */
    public void setVariablesMap(final Hashtable < String, Object > variablesMap) {
        mVariablesMap = variablesMap;
    }

    /**
     * Store the value of a binding in the custom variables map for later
     * referral by custom code.
     * 
     * @param binding the current binding
     * @throws HostException if value cannot be stored
     */
    public void storeCustomVariable(final ICobolBinding binding)
            throws HostException {
        getVariablesMap().put(binding.getBindingName(),
                binding.getObjectValue(binding.getJaxbType()));

    }

    /**
     * This deals with a special case of elements which have a dependingOn
     * clause but their maxOccurs is 1. As such, they are not arrays but the
     * associated dependingOn counter determines if the element exist or not
     * (optional items).
     * <p/>
     * If existence depends on a a counter, check counter value first. If the
     * associated counter is zero, then the object does not exist (should not be
     * visited as it has no bytes in the host payload or no java value object).
     * 
     * @param ce the binding object
     * @return true if object exists (has associated bytes in the incoming host
     *         payload or a non-null outbound java object value).
     * @throws HostException if existence test fails
     */
    public boolean exists(final ICobolBinding ce) throws HostException {
        /*  */
        if (ce.getDependingOn() != null
                && ce.getDependingOn().length() > 0
                && ce.getMaxOccurs() == 1
                && ce.getParentBinding().getCounterValue(ce.getDependingOn()) == 0) {
            if (_log.isDebugEnabled()) {
                _log.debug("Visiting aborted for binding "
                        + ce.getBindingName() + ", it depends on "
                        + ce.getDependingOn() + " which is zero");
            }
            return false;
        }
        return true;

    }

    /**
     * COBOL items are usually expected at a certain offset (fixed position).
     * Most of the time, this offset is where the previous item left but
     * sometimes, a virtual filler must be accounted for.
     * <p/>
     * Note that the virtual offset contributes to the offset only if there is a
     * following, fixed position, item. It must not be added to the last item in
     * a structure.
     * <p/>
     * This is a destructive method that resets the virtual filler length to
     * zero.
     * 
     * @return returns the offset where unmarshaling should start for the next
     *         item.
     */
    public int getStartOffset() {
        int startOffset = getOffset();
        if (_virtualFillerLength > 0) {
            startOffset += _virtualFillerLength;
            _virtualFillerLength = 0;
        }
        return startOffset;
    }

    /**
     * Given a choice, evaluate the largest alternative size.
     * 
     * @param ce the choice binding
     * @return the size of the largest alternative
     * @throws HostException if size cannot be evaluated
     */
    public int getMaxAlternaliveLength(ICobolChoiceBinding ce)
            throws HostException {
        int maxAlternaliveLength = 0;
        for (ICobolBinding alternative : ce.getAlternativesList()) {
            if (alternative.getByteLength() > maxAlternaliveLength) {
                maxAlternaliveLength = alternative.getByteLength();
            }
        }
        return maxAlternaliveLength;
    }

    /**
     * Default alternative selection logic.
     * <p/>
     * Every alternative is given a chance. If it throws an exception, the next
     * alternative is tried. If it does not increment the offset, the next
     * alternative is tried.
     * <p/>
     * If none of the alternatives worked, an exception is raised.
     * 
     * @param ce the choice binding
     * @return an alternative that was successfully visited
     * @throws HostException if no alternative could be chosen
     */
    public ICobolBinding chooseDefaultAlternative(ICobolChoiceBinding ce)
            throws HostException {
        ICobolBinding chosenAlternative = null;
        int savedOffset = getOffset();
        for (ICobolBinding alt : ce.getAlternativesList()) {
            try {
                if (isCandidateAlternative(alt)) {
                    alt.accept(this);
                } else {
                    continue;
                }
                if (savedOffset < getOffset()) {
                    chosenAlternative = alt;
                    break;
                }
            } catch (HostException he) {
                setOffset(savedOffset);
            }
        }
        if (chosenAlternative == null) {
            throw new HostException("No alternative found for choice element "
                    + ce.getBindingName());
        }
        return chosenAlternative;
    }

    /**
     * Should an alternative be considered for visiting.
     * <p/>
     * Used with the default choice selection.
     * <p/>
     * By default all alternatives are candidate.
     * 
     * @param alt the proposed candidate alternative
     * @return true if this alternative should be considered for visiting
     */
    public boolean isCandidateAlternative(ICobolBinding alt) {
        return true;
    }

    /**
     * If chosen alternative is shorter than the max, keep record of the
     * difference because next item (if any) is not variably located. We might
     * already have a virtual offset, left over from an inner choice.
     * 
     * @param ce the choice binding
     * @param chosenAlternative the chosen alternative
     * @throws HostException if size cannot be evaluated
     */
    public void setChosenAlternative(ICobolChoiceBinding ce,
            ICobolBinding chosenAlternative) throws HostException {
        int maxAlternaliveLength = getMaxAlternaliveLength(ce);
        if (chosenAlternative.getByteLength() < maxAlternaliveLength) {
            _virtualFillerLength += maxAlternaliveLength
                    - chosenAlternative.getByteLength();
        }
    }
}
