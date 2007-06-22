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

import java.io.File;

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
import com.legstar.host.HostException;
import com.legstar.xslt.XSLTException;

/**
 * This class implements the visitor pattern in order to reflect a JAXB object
 * tree instance into an XML format. Each complex element, choice element
 * and complex array generates a separate XML file that describes the elements
 * and can be later used by XSL to generate code.
 *
 * @author Fady Moussallam
 * 
*/
public class CoxbReflectVisitor extends CobolElementVisitor {

	/** All classes reflected upon belong to this package. */
	private String mPackageName;
	
	/** Class used to create source files. */
	private CoxbWriter mWriter;
	
	
	/**
	 * Constructor.
     * 
	 * @param packageName the package name used for all generated binding
	 *  classes
	 * @param targetDir the target directory for generated files
	 * @throws HostException if directory provided is not accessible
	 */
	public CoxbReflectVisitor(
			final String packageName,
			final File targetDir) throws HostException {
		mPackageName = packageName;
		mWriter = new CoxbWriter(targetDir);
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolComplexBinding ce)
		throws HostException {
		
		/* If this is the child of a parent complex item, first insert a
		 *  property description in parent XML*/
		if (ce.getParentBinding() != null) {
			mWriter.write(CoxbFormatter.formatProperty(ce));
		}
		
		/* Each complex element goes in its own XML file*/
		String tempFile = mWriter.openWrite(
				CoxbFormatter.formatComplexProlog(ce, mPackageName));
		
		/* Create an ordered list of properties in this complex element.
		 * Since we are unmarshaling, bound objects  do not pre-exist so
		 * we need to create them. */
		ce.createJaxbObject();
		java.util.List < ICobolBinding > children =
			ce.getChildrenList();
		
		/* Iteratively propagate the accept on complex element children.
		 * The order in which children are processed is important. */
		for (ICobolBinding child : children) {
			child.accept(this);
		}
		mWriter.writeClose(CoxbFormatter.formatComplexEpilog());
		
		/* Apply XSL transform on the XML just formatted */
		CoxbBinding cb;
		try {
			cb = new CoxbBinding();
			cb.createBinding(tempFile, mWriter.getTargetDirName());
		} catch (XSLTException e) {
			throw (new HostException("XSLTException " + e.getMessage()));
		}
		return;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolChoiceBinding ce)
		throws HostException {
		/* First insert a property description in parent XML */
		mWriter.write(CoxbFormatter.formatProperty(ce));

		/* Create a separate XML to describe alternatives */
		String tempFile = mWriter.openWrite(
				CoxbFormatter.formatComplexProlog(ce, mPackageName));
		/* We want to reflect on all alternatives */
		for (ICobolBinding alt : ce.getAlternativesList()) {
			try {
				alt.accept(this);
			} catch (HostException he) {
				continue;
			}
		}
		mWriter.writeClose(CoxbFormatter.formatComplexEpilog());

		/* Apply XSL transform on the XML just formatted */
		CoxbBinding cb;
		try {
			cb = new CoxbBinding();
			cb.createBinding(tempFile, mWriter.getTargetDirName());
		} catch (XSLTException e) {
			throw (new HostException("XSLTException " + e.getMessage()));
		}
		return;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayComplexBinding ce)
		throws HostException {
		/* First insert a property description in parent XML */
		mWriter.write(CoxbFormatter.formatProperty(ce));

		String tempFile = mWriter.openWrite(
				CoxbFormatter.formatComplexProlog(ce, mPackageName));
		/* We reflect on only one item of the array */
		ce.createJaxbObject();
		ICobolBinding itemDesc = ce.getComplexItemBinding();
		itemDesc.accept(this);
		mWriter.writeClose(CoxbFormatter.formatComplexEpilog());

		/* Apply XSL transform on the XML just formatted */
		CoxbBinding cb;
		try {
			cb = new CoxbBinding();
			cb.createBinding(tempFile, mWriter.getTargetDirName());
		} catch (XSLTException e) {
			throw (new HostException("XSLTException " + e.getMessage()));
		}
		return;
	}
	
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolStringBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayStringBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolNationalBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayNationalBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolZonedDecimalBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayZonedDecimalBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolPackedDecimalBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayPackedDecimalBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolBinaryBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayBinaryBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolFloatBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayFloatBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}

	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolDoubleBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayDoubleBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolOctetStreamBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}
	/** {@inheritDoc} */
	@Override
	public final void visit(final ICobolArrayOctetStreamBinding ce)
		throws HostException {
		mWriter.write(CoxbFormatter.formatProperty(ce));
		return;
	}

	/**
	 * @return the JAXB classes reflected upon package name
	 */
	public final String getPackageName() {
		return mPackageName;
	}

	/**
	 * @param packageName the package name to set
	 */
	public final void setPackageName(final String packageName) {
		mPackageName = packageName;
	}

	/**
	 * @return the Writer instance
	 */
	public final CoxbWriter getWriter() {
		return mWriter;
	}

	/**
	 * @param writer the Writer instance to set
	 */
	public final void setWriter(final CoxbWriter writer) {
		mWriter = writer;
	}
	
}
