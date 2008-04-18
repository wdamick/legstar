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
package com.legstar.coxb.gen;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolArrayBinaryBinding;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.coxb.ICobolArrayFloatBinding;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.ICobolArrayPackedDecimalBinding;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.ICobolBinaryBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.host.HostException;

/**
 * This class implements the visitor pattern in order to reflect on a JAXB
 * object tree instance. Each complex element, choice element and complex 
 * array generates a separate java class which code is generated using
 * appropriate velocity templates.
 *
 * @author Fady Moussallam
 * 
*/
public class CoxbGenReflectVisitor extends CobolElementVisitor {

	/** Actual code generator. */
	private CoxbGenWriter mWriter;
	
	/**
	 * Constructor.
     * 
	 * @param coxbGenContext set of parameters
	 * @throws HostException if directory provided is not accessible
	 */
	public CoxbGenReflectVisitor(
			final CoxbGenContext coxbGenContext) throws HostException {
		try {
			mWriter = new CoxbGenWriter(coxbGenContext);
		} catch (CodeGenException e) {
			throw new HostException(e);
		}
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolComplexBinding ce)
		throws HostException {
		
		/* Create an ordered list of properties in this complex element.
		 * Since we are unmarshaling, bound objects do not pre-exist so
		 * we need to create them. */
		ce.createValueObject();
		java.util.List < ICobolBinding > children =
			ce.getChildrenList();
		
		/* Iteratively propagate the accept on complex element children.
		 * The order in which children are processed is important. */
		for (ICobolBinding child : children) {
			child.accept(this);
		}
		
		/* Now that all children have been processed, its time to generate
		 * this element binding*/
		try {
			mWriter.write(ce);
		} catch (CodeGenException e) {
			throw new HostException(e);
		}
		return;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolChoiceBinding ce)
		throws HostException {

		/* We want to reflect on all alternatives */
		for (ICobolBinding alt : ce.getAlternativesList()) {
			try {
				alt.accept(this);
			} catch (HostException he) {
				continue;
			}
		}
		/* Now that all alternatives have been processed, its time to generate
		 * this element binding*/
		try {
			mWriter.write(ce);
		} catch (CodeGenException e) {
			throw new HostException(e);
		}
		return;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayComplexBinding ce)
		throws HostException {
		/* We reflect on only one item of the array */
		ce.createValueObject();
		ICobolBinding itemDesc = ce.getComplexItemBinding();
		itemDesc.accept(this);
		
		/* Now that an item has been processed, its time to generate
		 * this element binding*/
		try {
			mWriter.write(ce);
		} catch (CodeGenException e) {
			throw new HostException(e);
		}
		return;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolStringBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayStringBinding ce)
		throws HostException {
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolNationalBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayNationalBinding ce)
		throws HostException {
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolZonedDecimalBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayZonedDecimalBinding ce)
		throws HostException {
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolPackedDecimalBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayPackedDecimalBinding ce)
		throws HostException {
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolBinaryBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayBinaryBinding ce)
		throws HostException {
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolFloatBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayFloatBinding ce)
		throws HostException {
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolDoubleBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayDoubleBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolOctetStreamBinding ce)
		throws HostException {
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayOctetStreamBinding ce)
		throws HostException {
		return;
	}

}
