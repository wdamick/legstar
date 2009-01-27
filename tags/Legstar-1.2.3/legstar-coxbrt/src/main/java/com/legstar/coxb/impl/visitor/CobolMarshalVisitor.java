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
package com.legstar.coxb.impl.visitor;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolArrayBinaryBinding;
import com.legstar.coxb.ICobolArrayComplexBinding;
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
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.host.HostException;

/**
 * This class implements the visitor pattern in order to marshal a java object
 * tree instance into a host data buffer.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolMarshalVisitor extends CobolElementVisitor {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(CobolMarshalVisitor.class);

    /** Visitor constructor.
     * @param hostBytes host buffer used by visitor
     * @param offset offset in host buffer
     * @param cobolConverters set of converters to use for cobol elements
     */
    public CobolMarshalVisitor(final byte[] hostBytes,
            final int offset,
            final ICobolConverters cobolConverters) {
        super(hostBytes, offset, cobolConverters);
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolComplexBinding ce)
    throws HostException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Marshaling started for complex binding "
                    + ce.getBindingName());
        }

        /* Ask complex binding to synchronize its children with the jaxb
         * bound object. */
        ce.setChildrenValues();

        /* If this complex element contains dynamic counters, their value
         * will typically not have been set by the previous setChildrenValues.
         * This is because they are not bound to a jaxb property. Their value
         * will be determined later when associated lists or arrays will be
         * marshalled. So here, we save the initial offset. */
        int startOffset = getOffset();

        /* Marshal all children (note that counters, if any,  will have zero
         * values) */
        for (ICobolBinding child : ce.getChildrenList()) {
            child.accept(this);
        }

        /* Now the counters values should be known, so we can re-marshal
         * those values at the start of the structure offset. */
        if (ce.getDynamicCountersCount() > 0) {
            int endOffset = getOffset();
            setOffset(startOffset);
            for (int i = 0; i < ce.getDynamicCountersCount(); i++) {
                ce.getChildrenList().get(i).accept(this);
            }
            setOffset(endOffset);
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("Marshaling successful for complex binding "
                    + ce.getBindingName());
        }
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolChoiceBinding ce)
    throws HostException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Marshaling started for choice binding "
                    + ce.getBindingName());
        }
        /* Ask choice binding to synchronize its alternatives state with the
         * jaxb bound object. */
        ce.setAlternativesValues();

        /* In a choice situation, only one alternative should be accepted when
         * this element is visited. The logic to determine which alternative 
         * should be selected is customizable via the
         * ICobolMarshalChoiceStrategy  interface.  If no direct selection of an
         * alternative is available, the default behavior is to select the first
         * alternative that is accepted without error and moved the offset. */
        boolean bAlternativeFound = false;

        /* If an external selector is provided, try it first */
        if (ce.getMarshalChoiceStrategy() != null) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Calling Marshal choice strategy  "
                        + ce.getMarshalChoiceStrategyClassName());
            }
            ICobolBinding alt =
                ce.getMarshalChoiceStrategy().choose(
                        ce, getVariablesMap(), this);
            /* If selector was successful, use the selected alternative */
            if (alt != null) {
                alt.accept(this);
                bAlternativeFound = true;
            }
        }

        /* Default behavior if direct selection was not possible */
        if (!bAlternativeFound) {
            for (ICobolBinding alt : ce.getAlternativesList()) {
                /* Only consider alternatives with bound values
                 * (They have been explicitly set)) */
                if (alt.isSet()) {
                    /* Save the visitor offset context */
                    int savedOffset = getOffset();
                    alt.accept(this);
                    /* If offset was succesfully incremented, we consider  we 
                     * found a valid alternative, just get out of the loop.*/
                    if (savedOffset < getOffset()) {
                        bAlternativeFound = true;
                        break;
                    }
                }
            }
        }

        /* If none of the alternatives moved the offset, raise an exception */
        if (!bAlternativeFound) {
            throw new HostException(
                    "No alternative found for choice element "
                    + ce.getBindingName());
        }
        if (LOG.isDebugEnabled()) {
            LOG.debug("Marshaling successful for choice binding "
                    + ce.getBindingName());
        }
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayComplexBinding ce)
    throws HostException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Marshaling started for array of complex bindings "
                    + ce.getBindingName());
        }
        /* Visit each item of the array in turn */
        for (int i = 0; i < ce.getObjectList().size(); i++) {
            ce.setItemValue(i);
            ICobolBinding itemDesc = ce.getComplexItemBinding();
            itemDesc.accept(this);
        }
        if (LOG.isDebugEnabled()) {
            LOG.debug("Marshaling successful for array of complex bindings "
                    + ce.getBindingName());
        }
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolStringBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolStringConverter().
                toHost(ce, getHostBytes(), getOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayStringBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolStringConverter().
                toHost(ce, getHostBytes(), getOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolNationalBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolNationalConverter().
                toHost(ce, getHostBytes(), getOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayNationalBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolNationalConverter().
                toHost(ce, getHostBytes(), getOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolZonedDecimalBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolZonedDecimalConverter().
                toHost(ce, getHostBytes(), getOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }

    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayZonedDecimalBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolZonedDecimalConverter().
                toHost(ce, getHostBytes(), getOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolPackedDecimalBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolPackedDecimalConverter().
                toHost(ce, getHostBytes(), getOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }

    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayPackedDecimalBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolPackedDecimalConverter().
                toHost(ce, getHostBytes(), getOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolBinaryBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolBinaryConverter().
                toHost(ce, getHostBytes(), getOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }

    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayBinaryBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolBinaryConverter().
                toHost(ce, getHostBytes(), getOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolFloatBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolFloatConverter().
                toHost(ce, getHostBytes(), getOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }

    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayFloatBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolFloatConverter().
                toHost(ce, getHostBytes(), getOffset(), ce.getCurrentOccurs()));
    }

    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolDoubleBinding ce)
    throws HostException {

        setOffset(getCobolConverters().
                getCobolDoubleConverter().
                toHost(ce, getHostBytes(), getOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayDoubleBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolDoubleConverter().
                toHost(ce, getHostBytes(), getOffset(), ce.getCurrentOccurs()));
    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolOctetStreamBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolOctetStreamConverter().
                toHost(ce, getHostBytes(), getOffset()));

        if (ce.isCustomVariable()) {
            storeCustomVariable(ce);
        }
    }
    /** {@inheritDoc} */
    @Override
    public final void visit(final ICobolArrayOctetStreamBinding ce)
    throws HostException {
        setOffset(getCobolConverters().
                getCobolOctetStreamConverter().
                toHost(ce, getHostBytes(), getOffset(), ce.getCurrentOccurs()));
    }
}
