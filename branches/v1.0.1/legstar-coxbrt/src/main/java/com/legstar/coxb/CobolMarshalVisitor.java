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
import com.legstar.coxb.convert.CobolConverters;
import com.legstar.host.HostException;

/**
 * This class implements the visitor pattern in order to marshal a java object
 * tree instance into a host data buffer.
 *
 * @author Fady Moussallam
 * 
*/
public class CobolMarshalVisitor extends CobolElementVisitor {
	
	/** Visitor constructor.
	 * @param hostBytes host buffer used by visitor
	 * @param offset offset in host buffer
	 * @param cobolConverters set of converters to use for cobol elements
	 */
	public CobolMarshalVisitor(final byte[] hostBytes,
			final int offset,
			final CobolConverters cobolConverters) {
		super(hostBytes, offset, cobolConverters);
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolComplexBinding ce)
		throws HostException {
		
		/* Make sure this complex element children are ready */
		ce.prepareChildren();
		
		/* If this element is not bound to a JAXB object, there is no
		 * data to mashal. Returning immediatly will keep the offset unchanged*/
		if (ce.getValue() == null) {
			return;
		}
		
		/* Ask complex binding to synchronize its internal state with the
		 * bound objects. */
		ce.getValuesFromBoundObject();
		
		/* Iteratively propagate the accept on complex element children.
		 * The order in which children are processed is important. */
		for (ICobolBinding child : ce.getChildrenList()) {
			child.accept(this);
		}
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolChoiceBinding ce)
		throws HostException {
		
		/* Make sure this choice element alternatives are ready */
		ce.prepareChildren();
		
		/* Ask choice binding to synchronize its internal state with the
		 * bound objects. */
		ce.getValuesFromBoundObject();
		
		/* In a choice situation, only one alternative should be accepted when
		 * this element is visited. The logic to determine which alternative 
		 * should be selected is customizable via the
		 * ICobolMarshalChoiceStrategy  interface.  If no direct selection of an
		 * alternative is available, the default behavior is to select the first
		 * alternative that is accepted without error and moved the offset. */
		boolean bAlternativeFound = false;
		
		/* If an external selector is provided, try it first */
		if (ce.getMarshalChoiceStrategy() != null) {
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
				/* Save the visitor offset context */
				int savedOffset = getOffset();
				alt.accept(this);
				/* When marshaling, a non-present alternative is signaled by
				 * an offset that does not change.  Otherwise, we consider we
				 * found a valid alternative, just get out of the loop. */
				if (savedOffset < getOffset()) {
					bAlternativeFound = true;
					break;
				}
			}
		}
		
		/* If none of the alternatives moved the offset, raise an exception */
		if (!bAlternativeFound) {
			throw new HostException(
					"No alternative found for choice element "
					+ ce.getJavaName());
		}
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayComplexBinding ce)
		throws HostException {

		/* Make sure this complex array element alternatives are ready */
		ce.prepareChildren();
		
		/* Visit each item of the array in turn */
		for (int i = 0; i < getCurrentOccurs(ce); i++) {
			ce.getValuesFromBoundItem(i);
			ICobolBinding itemDesc = ce.getItem(i);
			itemDesc.accept(this);
		}
		
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolStringBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolStringConverter().
				toHost(ce, getHostBytes(), getOffset()));

		/* If this element is needed as a custom variable, store its
		 * value in the custom variables map. */
		if (ce.isCustomVariable()) {
			getVariablesMap().put(
					ce.getJavaName(), ce.getValue());
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayStringBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolStringConverter().
				toHost(ce, getHostBytes(), getOffset(), getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolNationalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolNationalConverter().
				toHost(ce, getHostBytes(), getOffset()));

		/* If this element is needed as a custom variable, store its
		 * value in the custom variables map. */
		if (ce.isCustomVariable()) {
			getVariablesMap().put(
					ce.getJavaName(), ce.getValue());
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayNationalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolNationalConverter().
				toHost(ce, getHostBytes(), getOffset(), getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolZonedDecimalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolZonedDecimalConverter().
				toHost(ce, getHostBytes(), getOffset()));

		/* If this element is needed as a custom variable, store its
		 * value in the custom variables map. */
		if (ce.isCustomVariable()) {
			getVariablesMap().put(
					ce.getJavaName(), ce.getValue());
		}
		
		/* If this numeric element has been annotated as a custom variable, we
		 * need to save its value in the 'Occurs Depending On' map */
		if (ce.isODOObject()) {
			getODOMap().put(ce.getCobolName(), ce.getNumericValue());
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayZonedDecimalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolZonedDecimalConverter().
				toHost(ce, getHostBytes(), getOffset(), getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolPackedDecimalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolPackedDecimalConverter().
				toHost(ce, getHostBytes(), getOffset()));

		/* If this element is needed as a custom variable, store its
		 * value in the custom variables map. */
		if (ce.isCustomVariable()) {
			getVariablesMap().put(
					ce.getJavaName(), ce.getValue());
		}
		
		/* If this numeric element has been annotated as a custom variable, we
		 * need to save its value in the 'Occurs Depending On' map */
		if (ce.isODOObject()) {
			getODOMap().put(ce.getCobolName(), ce.getNumericValue());
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayPackedDecimalBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolPackedDecimalConverter().
				toHost(ce, getHostBytes(), getOffset(), getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolBinaryBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolBinaryConverter().
				toHost(ce, getHostBytes(), getOffset()));

		/* If this element is needed as a custom variable, store its
		 * value in the custom variables map. */
		if (ce.isCustomVariable()) {
			getVariablesMap().put(
					ce.getJavaName(), ce.getValue());
		}
		
		/* If this numeric element has been annotated as a custom variable, we
		 * need to save its value in the 'Occurs Depending On' map */
		if (ce.isODOObject()) {
			getODOMap().put(ce.getCobolName(), ce.getNumericValue());
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayBinaryBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolBinaryConverter().
				toHost(ce, getHostBytes(), getOffset(), getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolFloatBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolFloatConverter().
				toHost(ce, getHostBytes(), getOffset()));

		/* If this element is needed as a custom variable, store its
		 * value in the custom variables map. */
		if (ce.isCustomVariable()) {
			getVariablesMap().put(
					ce.getJavaName(), ce.getValue());
		}
		
		/* If this numeric element has been annotated as a custom variable, we
		 * need to save its value in the 'Occurs Depending On' map */
		if (ce.isODOObject()) {
			getODOMap().put(ce.getCobolName(), ce.getNumericValue());
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayFloatBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolFloatConverter().
				toHost(ce, getHostBytes(), getOffset(), getCurrentOccurs(ce)));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolDoubleBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolDoubleConverter().
				toHost(ce, getHostBytes(), getOffset()));

		/* If this element is needed as a custom variable, store its
		 * value in the custom variables map. */
		if (ce.isCustomVariable()) {
			getVariablesMap().put(
					ce.getJavaName(), ce.getValue());
		}
		
		/* If this numeric element has been annotated as a custom variable, we
		 * need to save its value in the 'Occurs Depending On' map */
		if (ce.isODOObject()) {
			getODOMap().put(ce.getCobolName(), ce.getNumericValue());
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayDoubleBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolDoubleConverter().
				toHost(ce, getHostBytes(), getOffset(), getCurrentOccurs(ce)));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolOctetStreamBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolOctetStreamConverter().
				toHost(ce, getHostBytes(), getOffset()));

		/* If this element is needed as a custom variable, store its
		 * value in the custom variables map. */
		if (ce.isCustomVariable()) {
			getVariablesMap().put(
					ce.getJavaName(), ce.getValue());
		}
		
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayOctetStreamBinding ce)
		throws HostException {
		setOffset(getCobolConverters().
				getCobolOctetStreamConverter().
				toHost(ce, getHostBytes(), getOffset(), getCurrentOccurs(ce)));
		return;
	}
}
