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
package com.legstar.coxb;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.convert.CobolConverters;
import com.legstar.host.HostException;

/**
 * This class implements the visitor pattern in order to marshal a java object
 * tree instance into a host data buffer.
 *
 * @author Fady Moussallam
 * 
*/
public class CobolUnmarshalVisitor extends CobolElementVisitor {
	
	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(CobolUnmarshalVisitor.class);

	/** Visitor constructor.
	 * @param hostBytes host buffer used by visitor
	 * @param offset offset in host buffer
	 * @param cobolConverters set of converters to use for cobol elements
	 */
	public CobolUnmarshalVisitor(final byte[] hostBytes,
			final int offset,
			final CobolConverters cobolConverters) {
		super(hostBytes, offset, cobolConverters);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolComplexBinding ce)
		throws HostException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Unmarshaling started for complex binding "
					+ ce.getJavaName());
		}
		/* Ask complex binding to create an empty jaxb bound object so it is
		 * ready for unmarshaling. */
		ce.createJaxbObject();
		
		/* Iteratively propagate the accept on complex element children.
		 * The order in which children are processed is important. */
		int index = 0;
		for (ICobolBinding child : ce.getChildrenList()) {
			child.accept(this);
			ce.setJaxbPropertyValue(index++);
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Unmarshaling successful for complex binding "
					+ ce.getJavaName());
		}
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolChoiceBinding ce)
		throws HostException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Unmarshaling started for choice binding "
					+ ce.getJavaName());
		}
		/* In a choice situation, only one alternative should be accepted when
		 * this element is visited. The logic to determine which alternative 
		 * should be selected is customizable via the
		 * ICobolUnmarshalChoiceStrategy interface.  If no direct selection of
		 * an alternative is available, the default behavior is to select the
		 * first alternative that is accepted without error and moved the
		 * offset. */
		boolean bAlternativeFound = false;
		
		/* If an external selector is provided, try it first */
		if (ce.getUnmarshalChoiceStrategy() != null) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Calling Unmarshal choice strategy  "
						+ ce.getUnmarshalChoiceStrategyClassName());
			}
			ICobolBinding alt =
				ce.getUnmarshalChoiceStrategy().choose(
						ce, getVariablesMap(), this);
			/* If selector was successful, use the selected alternative */
			if (alt != null) {
				alt.accept(this);
				bAlternativeFound = true;
				/* Ask choice binding to set bound object value from 
				 * unmarshaled data. */
				ce.setJaxbPropertyValue(
						ce.getAlternativesList().indexOf(alt));
			}
		}
		
		/* Default behavior if direct selection was not possible */
		if (!bAlternativeFound) {
			for (ICobolBinding alt : ce.getAlternativesList()) {
				/* Save the visitor offset context */
				int savedOffset = getOffset();
				try {
					alt.accept(this);
					/* When unmarshaling, a non present alternative is expected
					 * to result in an exception. Otherwise, we consider we
					 * found a valid alternative, just get out of the loop. */
					bAlternativeFound = true;
					/* Ask choice binding to set bound object value from 
					 * unmarshaled data. */
					ce.setJaxbPropertyValue(
							ce.getAlternativesList().indexOf(alt));
					break;
				} catch (HostException he) {
					setOffset(savedOffset);
				}
			}
		}
		
		/* If none of the alternatives worked, raise an exception */
		if (!bAlternativeFound) {
			throw new HostException(
					"No alternative found for choice element "
					+ ce.getJavaName());
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Unmarshaling successful for choice binding "
					+ ce.getJavaName());
		}
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayComplexBinding ce)
		throws HostException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("Unmarshaling started for array of complex bindings "
					+ ce.getJavaName());
		}
		/* Ask complex array binding to initialize bound array so that it is
		 * ready for unmarshaling. */
		ce.createJaxbObject();
		
		/* Visit each item of the array in turn */
		for (int i = 0; i < getCurrentOccurs(ce); i++) {
			ICobolBinding itemDesc = ce.getComplexItemBinding();
			itemDesc.accept(this);
			ce.addJaxbPropertyValue(i);
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Unmarshaling successful for array of complex bindings "
					+ ce.getJavaName());
		}
		
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolStringBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolStringConverter().
				fromHost(ce, getHostBytes(), getOffset()));

		if (ce.isCustomVariable()) {
			storeCustomVariable(ce);
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayStringBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolStringConverter().
				fromHost(ce, getHostBytes(), getOffset(),
						getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolNationalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolNationalConverter().
				fromHost(ce, getHostBytes(), getOffset()));

		if (ce.isCustomVariable()) {
			storeCustomVariable(ce);
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayNationalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolNationalConverter().
				fromHost(ce, getHostBytes(), getOffset(),
						getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolZonedDecimalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolZonedDecimalConverter().
				fromHost(ce, getHostBytes(), getOffset()));

		if (ce.isCustomVariable()) {
			storeCustomVariable(ce);
		}
		
		if (ce.isODOObject()) {
			storeODOValue(ce);
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayZonedDecimalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolZonedDecimalConverter().
				fromHost(ce, getHostBytes(), getOffset(),
						getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolPackedDecimalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolPackedDecimalConverter().
				fromHost(ce, getHostBytes(), getOffset()));

		if (ce.isCustomVariable()) {
			storeCustomVariable(ce);
		}
		
		if (ce.isODOObject()) {
			storeODOValue(ce);
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayPackedDecimalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolPackedDecimalConverter().
				fromHost(ce, getHostBytes(), getOffset(),
						getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolBinaryBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolBinaryConverter().
				fromHost(ce, getHostBytes(), getOffset()));

		if (ce.isCustomVariable()) {
			storeCustomVariable(ce);
		}
		
		if (ce.isODOObject()) {
			storeODOValue(ce);
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayBinaryBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolBinaryConverter().
				fromHost(ce, getHostBytes(), getOffset(),
						getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolFloatBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolFloatConverter().
				fromHost(ce, getHostBytes(), getOffset()));

		if (ce.isCustomVariable()) {
			storeCustomVariable(ce);
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayFloatBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolFloatConverter().
				fromHost(ce, getHostBytes(), getOffset(),
						getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolDoubleBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolDoubleConverter().
				fromHost(ce, getHostBytes(), getOffset()));

		if (ce.isCustomVariable()) {
			storeCustomVariable(ce);
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayDoubleBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolDoubleConverter().
				fromHost(ce, getHostBytes(), getOffset(),
						getCurrentOccurs(ce)));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolOctetStreamBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolOctetStreamConverter().
				fromHost(ce, getHostBytes(), getOffset()));

		if (ce.isCustomVariable()) {
			storeCustomVariable(ce);
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayOctetStreamBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolOctetStreamConverter().
				fromHost(ce, getHostBytes(), getOffset(),
						getCurrentOccurs(ce)));
		return;
	}
}
