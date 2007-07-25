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
	
	/** Current identation level. */
	private int mIndentFactor;
	
	/** New line characters. */
	private static final String CRLF = "\r\n";
	
	/** Used to build valid cobol names from java names. */
	private CobolNameResolver mNameResolver;
	
	/**
	 * Create a Cobol generator visitor.
	 * @param writer destination for the generated cobol source
	 * @throws HostException if generator cannot be created
	 */
	public CobolGenVisitor(
			final BufferedWriter writer) throws HostException {
		mWriter = writer;
		mIndentFactor = 0;
		try {
			mNameResolver = new CobolNameResolver();
		} catch (CobolNameResolverException e) {
			throw new HostException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(
			final ICobolComplexBinding ce) throws HostException {
		write(ce);
		mIndentFactor++;
		for (ICobolBinding cb : ce.getChildrenList()) {
			cb.accept(this);
		}
		mIndentFactor--;
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
			ce.setLevelNumber(mIndentFactor + 1);
			
			/* We also check that our cobol names are unique because
			 * this is simpler to manipulate in Cobol. */
			ce.setCobolName(mNameResolver.getUniqueName(ce.getCobolName()));
			
			mWriter.write(CobolGenFormatter.formatCobolClause(
					ce, mIndentFactor));
			mWriter.write(CRLF);
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
