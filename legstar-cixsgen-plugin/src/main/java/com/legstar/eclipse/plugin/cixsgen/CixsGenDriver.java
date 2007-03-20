/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.eclipse.plugin.cixsgen;

import java.io.File;
import java.io.IOException;
import java.io.FileWriter;
import java.io.InputStream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import com.legstar.cixs.gen.CixsOperation;
import com.legstar.cixs.gen.CixsService;

import com.legstar.eclipse.plugin.cixsgen.model.CixsGenDocument;
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
	
	/** Relative path to CIXS classes generation XSLT. */
	private static final String CIXS_XSL_LOC = "/ant/build-service.xml";
	
	/** Temporary ant file name. */
	private static final String TEMP_ANT_FILE_NAME = "cixsgen";

	/** Temporary ant file name suffix. */
	private static final String TEMP_ANT_FILE_SUFFIX = "xml";

	/** XML element that delineate a Service in the Ant script. */
	private static final String SERVICE_TAG = "<service";

	/** XML element that terminates the Cixsgen task in the Ant script. */
	private static final String CIXSGEN_END_TAG = "</cixsgen>";
	
	/** Start generation message text. */
	private static final String GEN_STARTED_MSG =
		"Generating CIXS classes for ";

	/** XSL not found error message text. */
	private static final String XSL_NOT_FOUND_MSG =
		"Unable to locate resource ";

	/**
	 * Constructor, loads the preferences.
	 */
	public CixsGenDriver() {
	}

	/**
	 * Generates JAXWS endpoint artifacts for a Web Service.
	 * @param serviceName the web service name
	 * @param cixsAntPropMap properties exoected by the Ant script
	 * @param cixsGenDoc Model properties describing Web Servive operations
	 * @param monitor a progress monitor
	 * @throws AntCreationException if ant script creation fails
	 * @throws CoreException if ant generation fails
	 */
	public final void generate(
			final String serviceName,
			final CixsAntPropMap cixsAntPropMap,
			final CixsGenDocument cixsGenDoc,
			final IProgressMonitor monitor)
		throws AntCreationException, CoreException {

		int scale = 1000;
		monitor.beginTask(GEN_STARTED_MSG + serviceName, 1 * scale);
		LegstarAntRunner antRunner = new LegstarAntRunner();
		
		/* Generate the CIXS classes */
		String[] cixsTargets = {CIXS_TARGET};
		antRunner.run(
				createBuild(serviceName, cixsGenDoc),
				cixsTargets,
				cixsAntPropMap.getMap(),
				monitor,
				scale);
	}
	
	/**
	 * Create a temporary file containing an ANT script to generate an
	 * endpoint. Because the generation task takes a complex set of inner
	 * objects as parameter, we create the set of parameters manually.
	 * @param wsName Web Service name
	 * @param cixsGenDoc Model properties describing Web Servive operations
	 * @return the name of the temporary file
	 * @throws AntCreationException if ant script creation failed
	 */
	private String createBuild(
			final String wsName,
			final CixsGenDocument cixsGenDoc) throws AntCreationException {
		File tmpFile = null;
		try {
			tmpFile = File.createTempFile(
					TEMP_ANT_FILE_NAME, TEMP_ANT_FILE_SUFFIX);
			StringBuffer result = new StringBuffer();
			InputStream xsltStream  =
				getClass().getResourceAsStream(CIXS_XSL_LOC);
			if (xsltStream == null) {
				throw (new AntCreationException(XSL_NOT_FOUND_MSG
						+ CIXS_XSL_LOC));
			}
			String content = readTextFile(xsltStream);
			result.append(content.substring(0, content.indexOf(SERVICE_TAG)));
			result.append(genCixsContent(wsName, cixsGenDoc).serialize());
			result.append(content.substring(content.indexOf(CIXSGEN_END_TAG)));
			FileWriter out = new FileWriter(tmpFile);
			out.write(result.toString());
	        out.close();
	     } catch (IOException e) {
			e.printStackTrace();
			throw (new AntCreationException("IOException " + e.getMessage()));
		}
		return tmpFile.getPath();
	}
	
	/**
	 * Reads a file content into a s String.
	 * 
	 * @param inStream the file content as a stream
	 * @return the resulting String
	 * @throws IOException if reading fails
	 */
	private static String readTextFile(
			final InputStream inStream) throws IOException {
		StringBuffer sb = new StringBuffer(1024);
		int cchar;
		while ((cchar = inStream.read()) > -1) {
			sb.append((char) cchar);
		}
		return sb.toString();
	}

	/**
	 * Generate the inner parameter passed to the generation ANT task. This
	 * is a hierarchy of service and operations descriptions.
	 * @param serviceName Web Service name
	 * @param cixsGenDoc Model properties describing Web Servive operations
	 * @return the parameter
	 */
	private CixsService genCixsContent(
			final String serviceName,
			final CixsGenDocument cixsGenDoc) {
		
		CixsgenPreferences cixsgenPref = new CixsgenPreferences();
		CixsService sv = new CixsService();
		sv.setServiceName(serviceName);
		sv.setEndpointPackageName(
				cixsgenPref.getCixsPackagePrefix() + '.' + serviceName);
		sv.setTargetNamespace(
				cixsgenPref.getCixsNamespacePrefix() + '/' + serviceName);

		int idx = 1;
		boolean opFound = true;
		while (opFound) {
			CixsOperation op = new CixsOperation();

			String sfx = "." + Integer.toString(idx);
			String opprop = CixsGenDocument.OP_NAME + sfx;
			if (cixsGenDoc.getProperty(opprop) != null
					&& cixsGenDoc.getProperty(opprop).length() > 0) {
				op.setOperationName(cixsGenDoc.getProperty(opprop));
				op.setProgramName(cixsGenDoc.getProperty(
						CixsGenDocument.OP_PROG + sfx, ""));
				op.setInputJaxbPackageName(cixsGenDoc.getProperty(
						CixsGenDocument.IN_PKG + sfx, ""));
				op.setInputJaxbType(cixsGenDoc.getProperty(
						CixsGenDocument.IN_TYPE + sfx, ""));
				op.setOutputJaxbPackageName(cixsGenDoc.getProperty(
						CixsGenDocument.OUT_PKG + sfx, ""));
				op.setOutputJaxbType(cixsGenDoc.getProperty(
						CixsGenDocument.OUT_TYPE + sfx, ""));

				sv.getOperations().add(op);

				opFound = true;
			} else {
				opFound = false;
			}
			idx += 1; 
		}
		return sv;
	}


}
