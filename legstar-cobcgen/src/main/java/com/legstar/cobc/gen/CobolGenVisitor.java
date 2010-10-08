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

import java.io.BufferedWriter;
import java.io.IOException;

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
import com.legstar.coxb.host.HostException;
import com.legstar.xsdc.gen.CobolNameResolver;
import com.legstar.xsdc.gen.CobolNameResolverException;
import com.legstar.coxb.ICobolBinding;

/**
 * This visitor traverses a jaxb object tree and creates a
 * complex Cobol data description from the Cobol annotations found
 * in these jaxb objects.
 */
public class CobolGenVisitor extends CobolElementVisitor {

    /** This writer receives the generated source code. */
    private BufferedWriter mWriter;

    /** First COBOL level. */
    private int mFirstCobolLevel;

    /** Current COBOL level. */
    private int mCurrentCobolLevel;

    /** Children level will be parent level plus this increment. */
    private int mCobolLevelIncrement;

    /** Used to build valid cobol names from java names. */
    private CobolNameResolver mNameResolver;

    /**
     * Create a Cobol generator visitor.
     * @param firstCobolLevel the first COBOL level in the generated structure
     * @param cobolLevelIncrement Children level will be parent level plus this
     *  increment (must be greater than 0)
     * @param writer destination for the generated cobol source
     * @throws HostException if generator cannot be created
     */
    public CobolGenVisitor(
            final int firstCobolLevel,
            final int cobolLevelIncrement,
            final BufferedWriter writer) throws HostException {
        mWriter = writer;
        /* If the start cobol level is not 1, it must be a multiple of the
         * increment */
        int startCobolLevel = 0;
        if (firstCobolLevel == 1) {
            startCobolLevel = 1;
        } else {
            int rest = firstCobolLevel % cobolLevelIncrement;
            if (rest == 0) {
                startCobolLevel = firstCobolLevel;
            } else {
                startCobolLevel = firstCobolLevel + cobolLevelIncrement - rest;
            }
        }
        mFirstCobolLevel = startCobolLevel;
        mCurrentCobolLevel = startCobolLevel;
        mCobolLevelIncrement = cobolLevelIncrement;
        try {
            mNameResolver = new CobolNameResolver();
        } catch (CobolNameResolverException e) {
            throw new HostException(e);
        }
    }

    /**
     * Create a Cobol generator visitor.
     * @param writer destination for the generated cobol source
     * @throws HostException if generator cannot be created
     */
    public CobolGenVisitor(
            final BufferedWriter writer) throws HostException {
        this(1, 1, writer);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolComplexBinding ce) throws HostException {
        write(ce);
        mCurrentCobolLevel += mCobolLevelIncrement;
        for (ICobolBinding cb : ce.getChildrenList()) {
            cb.accept(this);
        }
        mCurrentCobolLevel -= mCobolLevelIncrement;
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolChoiceBinding ce) throws HostException {
        for (ICobolBinding cb : ce.getAlternativesList()) {
            cb.accept(this);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayComplexBinding ce) throws HostException {
        ce.getComplexItemBinding().accept(this);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolStringBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayStringBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolNationalBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayNationalBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolDbcsBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayDbcsBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolZonedDecimalBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayZonedDecimalBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolPackedDecimalBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayPackedDecimalBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolBinaryBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayBinaryBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolFloatBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayFloatBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolDoubleBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayDoubleBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolOctetStreamBinding ce) throws HostException {
        write(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(
            final ICobolArrayOctetStreamBinding ce) throws HostException {
        write(ce);
    }

    /**
     * Adds a cobol sentence to the writer.
     * @param ce the cobol binding
     * @throws HostException if write fails
     */
    private void write(final ICobolBinding ce) throws HostException {
        try {
            /* Because level numbers returned by xsdCobolAnnotator are
             * not reliable, we substitute our own here. */
            ce.setLevelNumber(mCurrentCobolLevel);

            /* We also check that our cobol names are unique because
             * this is simpler to manipulate in Cobol. */
            ce.setCobolName(mNameResolver.getUniqueName(ce.getCobolName()));

            mWriter.write(CobolGenFormatter.formatCobolClause(
                    ce, getIndentFactor(
                            mFirstCobolLevel,
                            mCurrentCobolLevel,
                            mCobolLevelIncrement)));
            mWriter.newLine();
        } catch (IOException e) {
            throw new HostException(e);
        } catch (CobolNameResolverException e) {
            throw new HostException(e);
        }
    }

    /**
     * Calculates how many white space characters should be added to indent an
     * element in the data structure.
     * <p/>
     * Data items with indent factor of 0 start at column 8.
     * Data items with indent factor of 1 start at column 12 and so forth.
     * <p/>
     * Only data items with COBOL level 1 start at column 8.
     * @param firstCobolLevel the cobol level of the root element
     * @param currentCobolLevel the current element cobol level
     * @param cobolLevelIncrement the cobol level increment
     * @return the number of white space characters to prepend to data
     *  description.
     */
    private int getIndentFactor(
            final int firstCobolLevel,
            final int currentCobolLevel,
            final int cobolLevelIncrement) {
        int factor = (currentCobolLevel  - firstCobolLevel) / cobolLevelIncrement;
        if (firstCobolLevel == 1) {
            return factor;
        } else {
            return ++factor;
        }
    }

    /**
     * @return the Cobol Name Resolver
     */
    public CobolNameResolver getNameResolver() {
        return mNameResolver;
    }

}
