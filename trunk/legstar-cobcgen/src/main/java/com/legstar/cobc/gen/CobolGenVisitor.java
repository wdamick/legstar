/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.cobc.gen;

import java.io.BufferedWriter;
import java.io.IOException;

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
	 * @param startCobolLevel the first COBOL level in the generated structure
	 * @param cobolLevelIncrement Children level will be parent level plus this
	 *  increment (must be greater than 0)
	 * @param writer destination for the generated cobol source
	 * @throws HostException if generator cannot be created
	 */
	public CobolGenVisitor(
			final int startCobolLevel,
			final int cobolLevelIncrement,
			final BufferedWriter writer) throws HostException {
		mWriter = writer;
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
	public final void visit(
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
	public final void visit(final ICobolChoiceBinding ce) throws HostException {
		for (ICobolBinding cb : ce.getAlternativesList()) {
			cb.accept(this);
		}
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolArrayComplexBinding ce) throws HostException {
		ce.getComplexItemBinding().accept(this);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolStringBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolArrayStringBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolNationalBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolArrayNationalBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolZonedDecimalBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolArrayZonedDecimalBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolPackedDecimalBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolArrayPackedDecimalBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolBinaryBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolArrayBinaryBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolFloatBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolArrayFloatBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolDoubleBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolArrayDoubleBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolOctetStreamBinding ce) throws HostException {
		write(ce);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
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
					ce, (mCurrentCobolLevel - mFirstCobolLevel)
					/ mCobolLevelIncrement));
			mWriter.newLine();
		} catch (IOException e) {
			throw new HostException(e);
		} catch (CobolNameResolverException e) {
			throw new HostException(e);
		}
	}

	/**
	 * @return the Cobol Name Resolver
	 */
	public final CobolNameResolver getNameResolver() {
		return mNameResolver;
	}

}
