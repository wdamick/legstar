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
package com.legstar.eclipse.plugin.cixsgen.jaxws;

import java.io.File;
import java.io.IOException;
import java.io.FileWriter;
import java.io.InputStream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import com.legstar.cixs.jaxws.model.CixsJaxwsService;

import com.legstar.eclipse.plugin.cixsgen.AntCreationException;
import com.legstar.eclipse.plugin.common.LegstarAntRunner;


/**
 * Legacy Web Service generation driver. This module dynamically creates an
 * ANT script based on the model passed to the execute method and then runs the
 * script to create the Web Services artifacts.
 *
 * @author Fady Moussallam
 * 
 */
public class CixsGenDriver {

	/** Target for Legacy Web Service generation (avoid compilation). */
	private static final String CIXS_TARGET = "generateService";
	
	/** Relative path to ANT template generation script. */
	private static final String CIXS_ANT_TEMPLATE_LOC =
		"/ant/build-jaxws-service.template";
	
	/**  Suffix of generated ANT script file. */
	private static final String ANT_FILE_SUFFIX = "xml";

	/** Prefix of generated ANT script file. */
	private static final String ANT_FILE_PREFIX = "build-";
	
	/** Start generation message text. */
	private static final String GEN_STARTED_MSG =
		"Generating CIXS classes for ";

	/** Resource not found error message text. */
	private static final String RESOURCE_NOT_FOUND_MSG =
		"Unable to locate resource ";

	/**
	 * Constructor.
	 */
	public CixsGenDriver() {
	}

	/**
	 * Generates JAXWS endpoint artifacts for a Web Service.
	 * @param serviceName the web service name
	 * @param cixsGenDescriptor properties expected by the Ant script
	 * @param service XML describing Web Service
	 * @param monitor a progress monitor
	 * @throws AntCreationException if ant script creation fails
	 * @throws CoreException if ant generation fails
	 */
	public final void generate(
			final String serviceName,
			final CixsGenDescriptor cixsGenDescriptor,
			final CixsJaxwsService service,
			final IProgressMonitor monitor)
		throws AntCreationException, CoreException {

		int scale = 1000;
		monitor.beginTask(GEN_STARTED_MSG + serviceName, 1 * scale);
		LegstarAntRunner antRunner = new LegstarAntRunner();
		
		/* Create a new ANT script file (replace existing one) */
		File scriptFile = new File(
				cixsGenDescriptor.getCixsAntScriptsDir() + File.separatorChar 
				+ ANT_FILE_PREFIX + serviceName + '.'
				+ ANT_FILE_SUFFIX); 
		createBuild(cixsGenDescriptor, service, scriptFile);
		
		/* Generate the CIXS classes by submitting the ANT script */
		String[] cixsTargets = {CIXS_TARGET};
		antRunner.run(
				scriptFile.getPath(),
				cixsTargets,
				null,
				monitor,
				scale);
	}
	
	/**
	 * Writes an ANT script in a file. The result script has steps to generate
	 * an endpoint. Because the generation ANT task takes a complex set of inner
	 * objects as parameters, the script is built dynamically.
	 * @param cixsGenDescriptor properties expected by the Ant script
	 * @param service Service descriptor
	 * @param scriptFile the target ANT script file
	 * @throws AntCreationException if ant script creation failed
	 */
	private void createBuild(
			final CixsGenDescriptor cixsGenDescriptor,
			final CixsJaxwsService service,
			final File scriptFile) throws AntCreationException {
		try {
			/* Load the template ANT script */
			InputStream templateStream  =
				getClass().getResourceAsStream(CIXS_ANT_TEMPLATE_LOC);
			if (templateStream == null) {
				throw (new AntCreationException(RESOURCE_NOT_FOUND_MSG
						+ CIXS_ANT_TEMPLATE_LOC));
			}
			String script = readTextFile(templateStream);
			
			/* Replace variables from template with actual values */
			script = script.replace("${base.dir}",
					cixsGenDescriptor.getCixsgenLocation());
			script = script.replace("${jaxb.bin.dir}",
					cixsGenDescriptor.getCixsJaxbBinariesDir());
			script = script.replace("${coxb.bin.dir}",
					cixsGenDescriptor.getCixsCoxbBinariesDir());
			script = script.replace("${cixs.src.dir}",
					cixsGenDescriptor.getCixsSourcesDir());
			script = script.replace("${wdd.dir}",
					cixsGenDescriptor.getCixsWebDescriptorsDir());
			script = script.replace("${ant.dir}",
					cixsGenDescriptor.getCixsAntScriptsDir());
			script = script.replace("${prop.dir}",
					cixsGenDescriptor.getCixsPropertiesDir());
			script = script.replace("${war.dir}",
					cixsGenDescriptor.getCixsWarDir());
			script = script.replace("${cixs.bin.dir}",
					cixsGenDescriptor.getCixsBinariesDir());
			script = script.replace("${cust.bin.dir}",
					cixsGenDescriptor.getCixsCustBinariesDir());

			/* Append this service serialization as XML */
			script = script.replace("${cixsService}", service.serialize());

			FileWriter out = new FileWriter(scriptFile);
			out.write(script);
	        out.close();
	     } catch (IOException e) {
			throw (new AntCreationException(e));
		}
	}
	
	/**
	 * Reads a file content into a String.
	 * 
	 * @param inStream the file content as a stream
	 * @return the resulting String
	 * @throws IOException if reading fails
	 */
	private static String readTextFile(
			final InputStream inStream) throws IOException {
        byte[] buffer = new byte[1024];
        StringBuffer result = new StringBuffer();
        int rc;
        while ((rc = inStream.read(buffer)) > 0) {
        	result.append(new String(buffer, 0, rc));
        }
        return result.toString();
	}
}
