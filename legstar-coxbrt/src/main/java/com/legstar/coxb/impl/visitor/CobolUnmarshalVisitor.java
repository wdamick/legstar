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
package com.legstar.coxb.impl.visitor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolArrayBinaryBinding;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolArrayDbcsBinding;
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.coxb.ICobolArrayFloatBinding;
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.ICobolArrayPackedDecimalBinding;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.ICobolBinaryBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolDbcsBinding;
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.host.HostException;

/**
 * This class implements the visitor pattern in order to marshal a java object
 * tree instance into a host data buffer.
 * 
 * @author Fady Moussallam
 * 
 */
public class CobolUnmarshalVisitor extends CobolElementVisitor {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * When this is not zero, it means the last COBOL item did not consume all
     * the bytes that it should have and yet the next item is not variably
     * located. In such a case we add a virtual filler length to the offset
     * before we start processing the next item.
     */
    private int _virtualFillerLength = 0;

    /**
     * Visitor constructor.
     * 
     * @param hostBytes host buffer used by visitor
     * @param offset offset in host buffer
     * @param cobolConverters set of converters to use for cobol elements
     */
    public CobolUnmarshalVisitor(final byte[] hostBytes, final int offset,
            final ICobolConverters cobolConverters) {
        super(hostBytes, offset, cobolConverters);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolComplexBinding ce) throws HostException {

        if (_log.isDebugEnabled()) {
            _log.debug("Unmarshaling started for complex binding "
                    + ce.getBindingName());
        }
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }

        /*
         * Ask complex binding to create an empty bound value object ready for
         * unmarshaling.
         */
        ce.createValueObject();

        /*
         * Iteratively propagate the accept on complex element children. The
         * order in which children are processed is important.
         */
        int index = 0;
        for (ICobolBinding child : ce.getChildrenList()) {
            child.accept(this);
            ce.setPropertyValue(index++);
        }
        if (_log.isDebugEnabled()) {
            _log.debug("Unmarshaling successful for complex binding "
                    + ce.getBindingName());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolChoiceBinding ce) throws HostException {

        if (_log.isDebugEnabled()) {
            _log.debug("Unmarshaling started for choice binding "
                    + ce.getBindingName());
        }

        /*
         * Make sure there are no leftovers from a previous use of this binding
         * and evaluate the maximum alternative length.
         */
        int maxAlternaliveLength = 0;
        for (ICobolBinding alternative : ce.getAlternativesList()) {
            alternative.setObjectValue(null);
            if (alternative.getByteLength() > maxAlternaliveLength) {
                maxAlternaliveLength = alternative.getByteLength();
            }
        }

        /*
         * In a choice situation, only one alternative should be accepted when
         * this element is visited. The logic to determine which alternative
         * should be selected is customizable via the
         * ICobolUnmarshalChoiceStrategy interface. If no direct selection of an
         * alternative is available, the default behavior is to select the first
         * alternative that is accepted without error and moved the offset.
         */
        ICobolBinding chosenAlternative = null;

        /* If an external selector is provided, try it first */
        if (ce.getUnmarshalChoiceStrategy() != null) {
            if (_log.isDebugEnabled()) {
                _log.debug("Calling Unmarshal choice strategy  "
                        + ce.getUnmarshalChoiceStrategyClassName());
            }
            chosenAlternative = ce.getUnmarshalChoiceStrategy().choose(ce,
                    getVariablesMap(), this);
            /* If selector was successful, use the selected alternative */
            if (chosenAlternative != null) {
                chosenAlternative.accept(this);
            }
        }

        /* Default behavior if direct selection was not possible */
        if (chosenAlternative == null) {
            for (ICobolBinding alt : ce.getAlternativesList()) {
                /* Save the visitor offset context */
                int savedOffset = getStartOffset();
                try {
                    alt.accept(this);
                    /*
                     * When unmarshaling, a non present alternative is expected
                     * to result in an exception. Otherwise, we consider we
                     * found a valid alternative, just get out of the loop.
                     */
                    chosenAlternative = alt;
                    break;
                } catch (HostException he) {
                    setOffset(savedOffset);
                }
            }
        }

        /* If none of the alternatives worked, raise an exception */
        if (chosenAlternative == null) {
            throw new HostException("No alternative found for choice element "
                    + ce.getBindingName());
        } else {
            /*
             * Ask choice binding to set bound object value from unmarshaled
             * data.
             */
            ce.setPropertyValue(ce.getAlternativesList().indexOf(
                    chosenAlternative));

            /*
             * If chosen alternative is shorter than the max, keep record of the
             * difference because next item is not variably located.
             */
            if (chosenAlternative.getByteLength() < maxAlternaliveLength) {
                setVirtualFillerLength(maxAlternaliveLength
                        - chosenAlternative.getByteLength());
            }
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Unmarshaling successful for choice binding "
                    + ce.getBindingName());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayComplexBinding ce) throws HostException {

        if (_log.isDebugEnabled()) {
            _log.debug("Unmarshaling started for array of complex bindings "
                    + ce.getBindingName());
        }
        /*
         * Ask complex array binding to initialize bound array so that it is
         * ready for unmarshaling.
         */
        ce.createValueObject();

        /* Visit each item of the array in turn */
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            ICobolBinding itemDesc = ce.getComplexItemBinding();
            itemDesc.accept(this);
            ce.addPropertyValue(i);
        }
        if (_log.isDebugEnabled()) {
            _log.debug("Unmarshaling successful for array of complex bindings "
                    + ce.getBindingName());
        }

    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolStringBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolStringConverter().fromHost(ce,
                getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayStringBinding ce) throws HostException {
        setOffset(getCobolConverters().getCobolStringConverter().fromHost(ce,
                getHostBytes(), getStartOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolNationalBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolNationalConverter().fromHost(ce,
                getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayNationalBinding ce) throws HostException {
        setOffset(getCobolConverters().getCobolNationalConverter().fromHost(ce,
                getHostBytes(), getStartOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolDbcsBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolDbcsConverter().fromHost(ce,
                getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayDbcsBinding ce) throws HostException {
        setOffset(getCobolConverters().getCobolDbcsConverter().fromHost(ce,
                getHostBytes(), getStartOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolZonedDecimalBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolZonedDecimalConverter()
                .fromHost(ce, getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayZonedDecimalBinding ce)
            throws HostException {
        setOffset(getCobolConverters().getCobolZonedDecimalConverter()
                .fromHost(ce, getHostBytes(), getStartOffset(),
                        ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolPackedDecimalBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolPackedDecimalConverter()
                .fromHost(ce, getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }

    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayPackedDecimalBinding ce)
            throws HostException {
        setOffset(getCobolConverters().getCobolPackedDecimalConverter()
                .fromHost(ce, getHostBytes(), getStartOffset(),
                        ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolBinaryBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolBinaryConverter().fromHost(ce,
                getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayBinaryBinding ce) throws HostException {
        setOffset(getCobolConverters().getCobolBinaryConverter().fromHost(ce,
                getHostBytes(), getStartOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolFloatBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolFloatConverter().fromHost(ce,
                getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayFloatBinding ce) throws HostException {
        setOffset(getCobolConverters().getCobolFloatConverter().fromHost(ce,
                getHostBytes(), getStartOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolDoubleBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolDoubleConverter().fromHost(ce,
                getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayDoubleBinding ce) throws HostException {
        setOffset(getCobolConverters().getCobolDoubleConverter().fromHost(ce,
                getHostBytes(), getStartOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolOctetStreamBinding ce) throws HostException {
        /* Object might be optional. Check if it should be visited. */
        if (!exists(ce)) {
            return;
        }
        setOffset(getCobolConverters().getCobolOctetStreamConverter().fromHost(
                ce, getHostBytes(), getStartOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayOctetStreamBinding ce)
            throws HostException {
        setOffset(getCobolConverters().getCobolOctetStreamConverter().fromHost(
                ce, getHostBytes(), getStartOffset(), ce.getCurrentOccurs()));
    }

    /**
     * COBOL items are expected at a certain offset in the incoming buffer. Most
     * of the time, this offset is where the previous item left but sometimes, a
     * virtual filler must be accounted for.
     * <p/>
     * This is a destructive method that resets the virtual filler length to
     * zero.
     * 
     * @return returns the offset where unmarshaling should start for the next
     *         item.
     */
    protected int getStartOffset() {
        int startOffset = getOffset();
        if (getVirtualFillerLength() > 0) {
            startOffset += getVirtualFillerLength();
            setVirtualFillerLength(0);
        }
        return startOffset;
    }

    /**
     * @return the length that needs to be added to the last offset to account
     *         for a previous item that processed less bytes than it should
     *         have.
     */
    public int getVirtualFillerLength() {
        return _virtualFillerLength;
    }

    /**
     * @param virtualFillerLength the virtual Filler Length to set
     */
    public void setVirtualFillerLength(final int virtualFillerLength) {
        _virtualFillerLength = virtualFillerLength;
    }

}
