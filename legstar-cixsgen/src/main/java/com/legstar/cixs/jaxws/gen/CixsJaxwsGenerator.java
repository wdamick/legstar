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
package com.legstar.cixs.jaxws.gen;

import java.io.File;
import org.apache.tools.ant.Task;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;

/**
 * @deprecated use <code>Jaxws2CixsGenerator</code> instead.
 * This Ant task creates the various Jaxws Service artifacts needed
 * for a complete Jaxws service with LegStar access capabilities.
 */
public class CixsJaxwsGenerator extends Task {

	/** CixsJaxwsGenerator is just a facade now. */
	private Jaxws2CixsGenerator mJaxws2CixsGenerator =
		new Jaxws2CixsGenerator();
	
	/** @{inheritDoc}*/
	@Override
	public final void init() {
		mJaxws2CixsGenerator.init();
	}

	/**
	 * Check that enough input parameters are set and then
	 * generate the requested artifacts.
	 * 
	 * */
	@Override
	public final void execute() {
		mJaxws2CixsGenerator.execute();
	}

	/**
	 * @return the Jaxws service 
	 */
	public final CixsJaxwsService getCixsJaxwsService() {
		return mJaxws2CixsGenerator.getCixsJaxwsService();
	}

	/**
	 * @param cixsJaxwsComponent the Jaxws service to set
	 */
	public final void setCixsJaxwsService(
			final CixsJaxwsService cixsJaxwsComponent) {
		mJaxws2CixsGenerator.setCixsService(cixsJaxwsComponent);
	}

	/**
	 * @param cixsJaxwsComponent the Jaxws service to set
	 */
	public final void add(final CixsJaxwsService cixsJaxwsComponent) {
		mJaxws2CixsGenerator.setCixsService(cixsJaxwsComponent);
	}

	/**
	 * @param cixsJaxwsComponent the Jaxws service to set
	 */
	public final void addCixsJaxwsService(
			final CixsJaxwsService cixsJaxwsComponent) {
		mJaxws2CixsGenerator.setCixsService(cixsJaxwsComponent);
	}

	/**
	 * @return the target source directory
	 */
	public final String getTargetSrcDir() {
		return mJaxws2CixsGenerator.getTargetSrcDir().toString();
	}

	/**
	 * @param targetSrcDir the target source directory to set
	 */
	public final void setTargetSrcDir(final String targetSrcDir) {
		mJaxws2CixsGenerator.setTargetSrcDir(new File(targetSrcDir));
	}

	/**
	 * @return the location for web deployment descriptors
	 */
	public final String getTargetWDDDir() {
		return mJaxws2CixsGenerator.getTargetWDDDir().toString();
	}

	/**
	 * @param targetWDDDir the location for web deployment descriptors
	 *  to set
	 */
	public final void setTargetWDDDir(
			final String targetWDDDir) {
		mJaxws2CixsGenerator.setTargetWDDDir(new File(targetWDDDir));
	}

	/**
	 * @return the location for ant deployment script
	 */
	public final String getTargetAntDir() {
		return mJaxws2CixsGenerator.getTargetAntDir().toString();
	}

	/**
	 * @param targetAntDir the location for ant deployment script to set
	 */
	public final void setTargetAntDir(final String targetAntDir) {
		mJaxws2CixsGenerator.setTargetAntDir(new File(targetAntDir));
	}

	/**
	 * @return the jaxws endpoint binaries
	 */
	public final String getCixsBinDir() {
		return mJaxws2CixsGenerator.getTargetBinDir().toString();
	}

	/**
	 * @param cixsBinDir the jaxws endpoint binaries to set
	 */
	public final void setCixsBinDir(final String cixsBinDir) {
		mJaxws2CixsGenerator.setTargetBinDir(new File(cixsBinDir));
	}

	/**
	 * @return the jaxb binaries location
	 */
	public final String getJaxbBinDir() {
		return mJaxws2CixsGenerator.getJaxbBinDir().toString();
	}

	/**
	 * @param jaxbBinDir the jaxb binaries location to set
	 */
	public final void setJaxbBinDir(final String jaxbBinDir) {
		mJaxws2CixsGenerator.setJaxbBinDir(new File(jaxbBinDir));
	}

	/**
	 * @return the coxb binaries location
	 */
	public final String getCoxbBinDir() {
		return mJaxws2CixsGenerator.getCoxbBinDir().toString();
	}

	/**
	 * @param coxbBinDir the coxb binaries location to set
	 */
	public final void setCoxbBinDir(final String coxbBinDir) {
		mJaxws2CixsGenerator.setCoxbBinDir(new File(coxbBinDir));
	}

	/**
	 * @return the target war files location
	 */
	public final String getTargetWarDir() {
		return mJaxws2CixsGenerator.getTargetWarDir().toString();
	}

	/**
	 * @param targetWarDir the target war files location to set
	 */
	public final void setTargetWarDir(final String targetWarDir) {
		mJaxws2CixsGenerator.setTargetWarDir(new File(targetWarDir));
	}

	/**
	 * @return custom binaries location
	 */
	public final String getCustBinDir() {
		return mJaxws2CixsGenerator.getCustBinDir().toString();
	}

	/**
	 * @param custBinDir the custom binaries location to set
	 */
	public final void setCustBinDir(final String custBinDir) {
		mJaxws2CixsGenerator.setCustBinDir(new File(custBinDir));
	}

	/**
	 * @return the target properties files location
	 */
	public final String getTargetPropDir() {
		return mJaxws2CixsGenerator.getTargetPropDir().toString();
	}

	/**
	 * @param targetPropDir the target properties files location to set
	 */
	public final void setTargetPropDir(final String targetPropDir) {
		mJaxws2CixsGenerator.setTargetPropDir(new File(targetPropDir));
	}


}
