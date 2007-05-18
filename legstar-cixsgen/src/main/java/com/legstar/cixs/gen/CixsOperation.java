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
package com.legstar.cixs.gen;

import java.util.ArrayList;
import java.util.List;

/**
 * This class describes a service operation and its binding to JAXB and CICS.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsOperation {

	/** Operation name. */
	private String mOperationName;

	/** Host program name. */
	private String mProgramName;

	/** CICS Channel name. */
	private String mChannel;
	
	/** Input structures. */
	private List < CixsStructure > mInputStructures =
		new ArrayList < CixsStructure >();;

	/** Output structures. */
	private List < CixsStructure > mOutputStructures =
		new ArrayList < CixsStructure >();;

	/**
	 * @return the service operation name
	 */
	public final String getOperationName() {
		return mOperationName;
	}

	/**
	 * @param operationName the service operation name to set
	 */
	public final void setOperationName(final String operationName) {
		mOperationName = operationName;
	}

	/**
	 * @return the host program name
	 */
	public final String getProgramName() {
		return mProgramName;
	}

	/**
	 * @param programName the host program name to set
	 */
	public final void setProgramName(final String programName) {
		mProgramName = programName;
	}

	/**
	 * @return the CICS Channel name
	 */
	public final String getChannel() {
		return mChannel;
	}

	/**
	 * @param channel the CICS Channel name to set
	 */
	public final void setChannel(final String channel) {
		mChannel = channel;
	}

	/**
	 * @return the Input structures
	 */
	public final List < CixsStructure > getInputStructures() {
		return mInputStructures;
	}

	/**
	 * @param inputStructures the Input structures to set
	 */
	public final void setInputStructures(
			final List < CixsStructure > inputStructures) {
		mInputStructures = inputStructures;
	}

	/**
	 * @param structure the structure to add
	 */
	public final void addInputStructure(final CixsStructure structure) {
		mInputStructures.add(structure);
	}
	
	/**
	 * @return the Output structures
	 */
	public final List < CixsStructure > getOutputStructures() {
		return mOutputStructures;
	}

	/**
	 * @param outputStructures the Output structures to set
	 */
	public final void setOutputStructures(
			final List < CixsStructure > outputStructures) {
		mOutputStructures = outputStructures;
	}

	/**
	 * @param structure the structure to add
	 */
	public final void addOutputStructure(final CixsStructure structure) {
		mOutputStructures.add(structure);
	}
	
}
