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
package com.legstar.schemagen;

import org.apache.tools.ant.Task;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Path;

import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.net.URI;
import java.util.Vector;

/**
 * This class implements an ant task to generate annotated XML schema from
 * a cobol file data descriptions.
 * 
 * @author Fady Moussallam
 * 
 */
/**
 * @author Fady
 *
 */
public class COXBSchemaGenerator extends Task  {

	
	/** Extension used when XML schema name is built. */
	private static final String XSD_EXT = ".xsd";
	
	/** True will produce extra traces. */
	private boolean mDebugMode = false;
	
	/** Creates a root element so that all simple elements at the 01
	 * level are included in it. */
	private String mRootName = null;
	
	/** Fully qualified file name of cobol file. */
	private String mCobolFile = null;
	
    /** Alternative way to specify cobol files using a path collection. */
	private Vector < Path > mCobolPaths = new Vector < Path >();
    
	/** Fully qualified file name of XML schema file. */
	private String mXSDFile = null;
	
	/** The target namespace for the XML schema elements. */
	private String mNamespace = null;
	
	/** The package name for generated Java classes. */
	private String mPackage = null;
	
	/** An instance of the JNI wrapper for Schema generation. */
	private COB2XSDJNIWrapper mSchemagen;
	
	/** No arg constructor. */
	public COXBSchemaGenerator() {
		mSchemagen = new COB2XSDJNIWrapper();
	}
	
	/**
	 *  The ant method. Generates an XML schema by invoking thru JNI the c
	 *  Cobol parser and XML schema generator.
	 */
    public final void execute() {
    	
    	/* Control inputs */
    	try {
			checkInput();
		} catch (URISyntaxException e) {
			throw (new BuildException(
			"The namespace " + mNamespace + " is invalid"));
		}
    	
		if (mCobolPaths.isEmpty()) {
			generateASchema(mCobolFile, mXSDFile, mNamespace, mPackage);
		} else {
			generateSchemas(mXSDFile);
		}
		
    }
    
    /**
     * Generates a single XML schema from a COBOL fragment.
     * @param cobolFile source file for COBOL fragment
     * @param xsdFile resulting XML schema file
     * @param namespace namespace to use as target for XSD
     * @param packageName the target package for generated JAXB classes
     */
    private void generateASchema(
    		final String cobolFile,
    		final String xsdFile,
    		final String namespace,
    		final String packageName) {
    	
		COB2XSDJNIWrapper.InVars inVars = mSchemagen.new InVars();
		COB2XSDJNIWrapper.OutVars outVars = mSchemagen.new OutVars();
		
		inVars.debugMode = mDebugMode;
		inVars.inRootName = mRootName;
        inVars.inFile = cobolFile;
        inVars.outFile = xsdFile;
		
		/* Not all possible options are exposed for now */
		inVars.xsdOptions = mSchemagen.new XsdOptions();
		inVars.cobolOptions = mSchemagen.new CobolOptions();
		
        inVars.xsdOptions.xsnsNs = namespace;
        inVars.xsdOptions.xsjaxbPackage = packageName;
        
        
		int resp = mSchemagen.cob2xsd(inVars, outVars);
		if (resp != 0) {
			throw (new BuildException(outVars.message));
			
		}
    }
    
    /**
     * A collection of cobol files from the path are processed. Each
     * generating a different XML schema.
     * @param xsdDir the target folder for XML schemas (should end with slash)
     */
    private void generateSchemas(final String xsdDir) {
        for (Iterator itPaths = mCobolPaths.iterator(); itPaths.hasNext();) {
            Path path = (Path) itPaths.next();
            String[] includedFiles = path.list(); 
            for (int i = 0; i < includedFiles.length; i++) {
                String pathname = includedFiles[i].replace('\\', '/');
                String filename = pathname.substring(
                		pathname.lastIndexOf("/") + 1);
                String xsdName;
                if (filename.lastIndexOf(".") > -1) {
                	xsdName = xsdDir + filename.substring(0,
                		filename.lastIndexOf(".")).toLowerCase() + XSD_EXT;
                } else {
                	xsdName = xsdDir + filename.toLowerCase() + XSD_EXT;
                }
                String namespace;
                if (mNamespace.charAt(mNamespace.length() - 1) != '/') {
                	namespace = mNamespace + '/' + filename.toLowerCase();
                } else {
                	namespace = mNamespace + filename.toLowerCase();
                }
                String packageName = mPackage + '.' + filename.toLowerCase();
                generateASchema(pathname, xsdName, namespace, packageName);
            }
        }
    }
    
    /**
     * Check the mandatory options.
     * 
     * @throws URISyntaxException if namespace provided is invalid
     */
    private void checkInput() throws URISyntaxException {
    	
    	if (mCobolFile == null || mCobolFile.length() == 0) {
    		if (mCobolPaths.isEmpty()) {
				throw (new BuildException(
					"You must specify a cobol file name or a path"));
    		}
    	} else {
    		if (!mCobolPaths.isEmpty()) {
				throw (new BuildException(
					"You must specify either a cobol file name"
					+ " or a path (but not both)"));
    		}
    	}
    	
    	if (mXSDFile == null || mXSDFile.length() == 0) {
			throw (new BuildException(
					"You must specify an output XML schema file name"));
    	}
    	
    	mXSDFile = mXSDFile.replace('\\', '/');
		if (!mCobolPaths.isEmpty()
				&& mXSDFile.charAt(mXSDFile.length() - 1) != '/') {
			throw (new BuildException(
				"XSD file should be a folder name (end with '/') when"
				+ " path option is used"));
		}

    	if (mNamespace == null || mNamespace.length() == 0) {
			throw (new BuildException(
					"You must specify an output XML schema namespace"));
    	}
    	
    	URI nURI = new URI(mNamespace);
    	if (nURI.isOpaque()) {
			throw (new BuildException(
			"Namespace " + mNamespace + " is not a hierarchical URI"));
    	}
    	
    	if (mPackage == null || mPackage.length() == 0) {
    		mPackage = packageFromURI(nURI);
    	}

    	if (mPackage == null || mPackage.length() == 0) {
			throw (new BuildException(
					"No valid package name is provided or can be derived"
					 + " from namespace"));
    	}
    }
    
    /**
     * Converts a URI into a package name. We assume a hierarchical,
     * server-based URI with the following syntax:
     *        [scheme:][//host[:port]][path][?query][#fragment]
     * The package name is derived from host, path and fragment.
     * 
     * @param namespaceURI the input namespace URI
     * @return the result package name
     */
    public static String packageFromURI(final URI namespaceURI) {
    	
    	StringBuilder result = new StringBuilder();
    	URI nURI = namespaceURI.normalize();
    	boolean firstToken = true;
    	
  		/* First part of package name is built from host with tokens in
   		 * reverse order. */
    	if (nURI.getHost() != null && nURI.getHost().length() != 0) {
        	Vector < String > v = new Vector < String >();
	    	StringTokenizer t = new StringTokenizer(nURI.getHost(), ".");
	   		while (t.hasMoreTokens()) {
	           v.addElement(t.nextToken());
	   		}
	   		
	   		for (int i = v.size(); i > 0; i--) {
	   			if (!firstToken) {
	   				result.append('.');
	   			} else {
	   				firstToken = false;
	   			}
	   			result.append(v.get(i - 1));
	   		}
    	}
   		
   		/* Next part of package is built from the path tokens */
    	if (nURI.getPath() != null && nURI.getPath().length() != 0) {
    		Vector < String > v = new Vector < String >();
   			StringTokenizer t = new StringTokenizer(nURI.getPath(), "/");
   	   		while (t.hasMoreTokens()) {
   	           v.addElement(t.nextToken());
   	   		}
   	   		
   	   		for (int i = 0; i < v.size(); i++) {
   	   			String token = v.get(i);
   	   			/* ignore situations such as /./../ */
   	   			if (token.equals(".") || token.equals("..")) {
   	   				continue;
   	   			}
	   			if (!firstToken) {
	   				result.append('.');
	   			} else {
	   				firstToken = false;
	   			}
   				result.append(v.get(i));
   	   		}
   		}
    	
   		/* Finally append any fragment */
    	if (nURI.getFragment() != null && nURI.getFragment().length() != 0) {
   			if (!firstToken) {
   				result.append('.');
   			} else {
   				firstToken = false;
   			}
			result.append(nURI.getFragment());
   		}
   		
   		/* By convention, namespaces are lowercase */
    	return result.toString().toLowerCase();
    }

	/**
	 * @return the debug mode
	 */
	public final boolean isDebugMode() {
		return mDebugMode;
	}

	/**
	 * @param debugMode the debug mode to set
	 */
	public final void setDebugMode(final boolean debugMode) {
		mDebugMode = debugMode;
	}

	/**
	 * @return the root name element to create if any
	 */
	public final String getRootName() {
		return mRootName;
	}

	/**
	 * @param rootName the root name element to set
	 */
	public final void setRootName(final String rootName) {
		mRootName = rootName;
	}

	/**
	 * @return the fully qualified name of the Cobol file to parse
	 */
	public final String getCobolFile() {
		return mCobolFile;
	}

	/**
	 * @param cobolFile the Cobol file name to set
	 */
	public final void setCobolFile(final String cobolFile) {
		mCobolFile = cobolFile;
	}

	/**
	 * @return the fully qualified XML schema file to produce
	 */
	public final String getXSDFile() {
		return mXSDFile;
	}

	/**
	 * @param file the XML schema file to set
	 */
	public final void setXSDFile(final String file) {
		/* Keep XSD files lowercase so we can derive project names
		 * from it if necessary. */
		mXSDFile = file.toLowerCase();
	}

	/**
	 * @return the XML schema target namespace
	 */
	public final String getNamespace() {
		return mNamespace;
	}

	/**
	 * @param namespace the XML schema target namespace to set
	 */
	public final void setNamespace(final String namespace) {
		/* Keep namespacelowercase so we can derive package names
		 * from it if necessary. */
		mNamespace = namespace.toLowerCase();
	}

	/**
	 * @return the generated Java classes package
	 */
	public final String getPackage() {
		return mPackage;
	}

	/**
	 * @param package1 the generated Java classes package to set
	 */
	public final void setPackage(final String package1) {
		mPackage = package1;
	}
 
	/**
	 * Add a path to the path list.
	 * @param path a path element containing cobol files
	 */
	public final void addPath(final Path path) {
		mCobolPaths.add(path);
    }

}
