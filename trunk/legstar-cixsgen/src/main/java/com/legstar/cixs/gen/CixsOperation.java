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

	/** JAXB type of input complex type. */
	private String mInputJaxbType;

	/** JAXB package of input complex type. */
	private String mInputJaxbPackageName;

	/** Strategy to resolve input choice alternatives. */
	private String mInputChoiceStrategy;

	/** JAXB type of output complex type. */
	private String mOutputJaxbType;

	/** JAXB package of output complex type. */
	private String mOutputJaxbPackageName;

	/** Strategy to resolve output choice alternatives. */
	private String mOutputChoiceStrategy;

	/**
	 * @return the the input JAXB package name
	 */
	public final String getInputJaxbPackageName() {
		return mInputJaxbPackageName;
	}

	/**
	 * @param inputJaxbPackageName the input JAXB package name to set
	 */
	public final void setInputJaxbPackageName(
			final String inputJaxbPackageName) {
		mInputJaxbPackageName = inputJaxbPackageName;
	}

	/**
	 * @return the input JAXB class name
	 */
	public final String getInputJaxbType() {
		return mInputJaxbType;
	}

	/**
	 * @param inputJaxbType the input JAXB class name to set
	 */
	public final void setInputJaxbType(final String inputJaxbType) {
		mInputJaxbType = inputJaxbType;
	}

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
	 * @return the output JAXB package name
	 */
	public final String getOutputJaxbPackageName() {
		return mOutputJaxbPackageName;
	}

	/**
	 * @param outputJaxbPackageName the output JAXB package name to set
	 */
	public final void setOutputJaxbPackageName(
			final String outputJaxbPackageName) {
		mOutputJaxbPackageName = outputJaxbPackageName;
	}

	/**
	 * @return the output jaxb class name
	 */
	public final String getOutputJaxbType() {
		return mOutputJaxbType;
	}

	/**
	 * @param outputJaxbType the output jaxb class name to set
	 */
	public final void setOutputJaxbType(final String outputJaxbType) {
		mOutputJaxbType = outputJaxbType;
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
	 * @return the the strategy to resolve input choice alternatives
	 */
	public final String getInputChoiceStrategy() {
		return mInputChoiceStrategy;
	}

	/**
	 * @param inputChoiceStrategy the strategy to resolve input choice
	 * alternatives to set
	 */
	public final void seInputChoiceStrategy(
			final String inputChoiceStrategy) {
		mInputChoiceStrategy = inputChoiceStrategy;
	}

	/**
	 * @return the strategy to resolve output choice alternatives
	 */
	public final String getOutputChoiceStrategy() {
		return mOutputChoiceStrategy;
	}

	/**
	 * @param outputChoiceStrategy the strategy to resolve output choice
	 * alternatives to set
	 */
	public final void setOutputChoiceStrategy(
			final String outputChoiceStrategy) {
		mOutputChoiceStrategy = outputChoiceStrategy;
	}
}
