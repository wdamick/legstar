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
package com.legstar.cobc.gen;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cobol.gen.CobolNameResolver;
import com.legstar.cobol.model.CobolDataItem;
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
import com.legstar.coxb.host.HostException;

/**
 * This visitor traverses a jaxb object tree and creates a complex Cobol data
 * description from the Cobol annotations found in these jaxb objects.
 */
public class CobolGenVisitor extends CobolElementVisitor {

    /**
     * This will contain the COBOL data model inferred from the JAXB
     * annotations.
     */
    private CobolDataItem rootDataItem;

    /** Current group data item. */
    private CobolDataItem groupDataItem;

    /** Previous group data item. */
    private CobolDataItem previousGroupDataItem;

    /** Current COBOL level. */
    private int currentCobolLevel;

    /** Children level will be parent level plus this increment. */
    private int cobolLevelIncrement;

    /** Used to build valid cobol names from java names. */
    private CobolNameResolver nameResolver;

    /** Logger. */
    private final Log logger = LogFactory.getLog(CobolGenVisitor.class);

    /**
     * Create a Cobol generator visitor.
     * 
     * @param firstCobolLevel the first COBOL level in the generated structure
     * @param cobolLevelIncrement Children level will be parent level plus this
     *            increment (must be greater than 0)
     * @throws HostException if name resolver cannot be created
     */
    public CobolGenVisitor(final int firstCobolLevel,
            final int cobolLevelIncrement) throws HostException {
        /*
         * If the start cobol level is not 1, it must be a multiple of the
         * increment
         */
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
        this.currentCobolLevel = startCobolLevel;
        this.cobolLevelIncrement = cobolLevelIncrement;
        this.nameResolver = new CobolNameResolver();
    }

    /**
     * Create a Cobol generator visitor.
     * 
     * @throws HostException if generator cannot be created
     */
    public CobolGenVisitor() throws HostException {
        this(1, 1);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolComplexBinding ce) throws HostException {
        CobolDataItem cobolDataItem = createCobolDataItem(ce);
        if (rootDataItem == null) {
            rootDataItem = cobolDataItem;
        }
        previousGroupDataItem = groupDataItem;
        groupDataItem = cobolDataItem;

        currentCobolLevel += cobolLevelIncrement;
        for (ICobolBinding cb : ce.getChildrenList()) {
            cb.accept(this);
        }
        currentCobolLevel -= cobolLevelIncrement;
        groupDataItem = previousGroupDataItem;
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
    public void visit(final ICobolArrayComplexBinding ce) throws HostException {
        ce.getComplexItemBinding().accept(this);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolStringBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayStringBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolNationalBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayNationalBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolDbcsBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayDbcsBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolZonedDecimalBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayZonedDecimalBinding ce)
            throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolPackedDecimalBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayPackedDecimalBinding ce)
            throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolBinaryBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayBinaryBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolFloatBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayFloatBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolDoubleBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayDoubleBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolOctetStreamBinding ce) throws HostException {
        createCobolDataItem(ce);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(final ICobolArrayOctetStreamBinding ce)
            throws HostException {
        createCobolDataItem(ce);
    }

    /**
     * Create a COBOL data item model from the JAXB annotations.
     * 
     * @param ce the cobol binding
     * @return a COBOL data item model object
     * @throws HostException if write fails
     */
    private CobolDataItem createCobolDataItem(final ICobolBinding ce)
            throws HostException {
        CobolDataItem cobolDataItem = new CobolDataItem(currentCobolLevel,
                nameResolver.getUniqueName(ce.getCobolName()));

        cobolDataItem.setDependingOn(ce.getDependingOn());
        cobolDataItem.setMaxOccurs(ce.getMaxOccurs());
        cobolDataItem.setMinOccurs(ce.getMinOccurs());
        cobolDataItem.setPicture(StringUtils.isBlank(ce.getPicture()) ? null
                : ce.getPicture());
        cobolDataItem
                .setRedefines(StringUtils.isBlank(ce.getRedefines()) ? null
                        : ce.getRedefines());
        cobolDataItem.setSrceLine(ce.getSrceLine());
        cobolDataItem.setCobolUsage(StringUtils.isBlank(ce.getUsage()) ? null
                : ce.getUsage());
        cobolDataItem.setValue(StringUtils.isBlank(ce.getDefaultValue()) ? null
                : ce.getDefaultValue());

        if (groupDataItem != null) {
            groupDataItem.getChildren().add(cobolDataItem);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("Created COBOL data item: " + cobolDataItem);
        }

        return cobolDataItem;

    }

    /**
     * @return the Cobol Name Resolver
     */
    public CobolNameResolver getNameResolver() {
        return nameResolver;
    }

    /**
     * @return the root COBOL Data Item
     */
    public CobolDataItem getRootDataItem() {
        return rootDataItem;
    }

}
