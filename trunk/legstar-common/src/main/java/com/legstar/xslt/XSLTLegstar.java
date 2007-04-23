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
package com.legstar.xslt;

import org.apache.tools.ant.Task;
import org.apache.tools.ant.BuildException;

import java.net.URL;
import java.util.Vector;

import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;


/**
 * This class implements a simple ant task which can be used as an alternative
 * to the XSLT ant task which does not solve XInclude.
 * 
 * @author Fady Moussallam
 * 
 */
public class XSLTLegstar extends Task {

	/** The output from the XSLT transform. */
	private String mResultfile;
	
	/** The XSLT transform file. */
	private String mXsltfile;
	
	/** The subject XML file. */
	private String mXmlfile;

	/** List of parameters for the XSLT transform. */
    private Vector < XSLTParameter > mParams = new Vector < XSLTParameter >();

    /** The classloader of this class. */
    private ClassLoader myClassLoader = null;

    /** Provided classpath for the classloader. */
    private Path mClasspath = null;
	
	/**
	 *  The ant method. Applies the XSLT transform on an XML file that
	 *  potentially contains XInclude instructions.
	 */
    public final void execute() {
    	long start = System.currentTimeMillis();
		XSLTransform t = new XSLTransform();
		/* XSLT style sheet is a resource within the jar */
		URL xsltResource  = getClass().getResource(mXsltfile);
		if (xsltResource == null) {
			throw (new BuildException("Unable to locate resource "
					+ mXsltfile));
		}
		t.transform(mXmlfile, xsltResource, mResultfile, mParams);
    	long end = System.currentTimeMillis();
  		System.out.println("Generation success for " + mXmlfile);
    	System.out.println("Duration=" + (end - start) + " ms");
    }

	/**
	 * @return Returns the result file name.
	 */
	public final String getResultfile() {
		return mResultfile;
	}

	/**
	 * @param filename The result file name to set.
	 */
	public final void setResultfile(final String filename) {
		mResultfile = new java.io.File(filename).toURI().toString();
	}

	/**
	 * @return Returns the xml file name.
	 */
	public final String getXmlfile() {
		return mXmlfile;
	}

	/**
	 * @param filename The xml file name to set.
	 */
	public final void setXmlfile(final String filename) {
		mXmlfile = filename;
	}

	/**
	 * @return Returns the XSLT template file name.
	 */
	public final String getXsltfile() {
		return mXsltfile;
	}

	/**
	 * @param filename The XSLT template file name to set.
	 */
	public final void setXsltfile(final String filename) {
		mXsltfile = filename;
	}

	/** Add a parameter to the parameter list.
	 * @return the new parameter */
	public final XSLTParameter createParam() {
		XSLTParameter param = new XSLTParameter();
		mParams.add(param);
        return param;
    }

    /**
     * Add the classpath.
     * @param path the classpath
     */
    public final void addClasspath(final Path path) {
        if (mClasspath != null) {
            throw new BuildException("<classpath> can be set only once.");
        }
        mClasspath = path;
    }

     /**
      * Creates a nested classpath element.
      * @return the new calsspath
      */
     public final Path createClasspath() {
         if (mClasspath == null) {
        	 mClasspath = new Path(getProject());
         }
         return mClasspath.createPath();
     }
     
     /**
      * Adds a reference to a CLASSPATH defined elsewhere.
      * @param reference a reference to a path ID
      */
     public final void setClasspathRef(final Reference reference) {
          createClasspath().setRefid(reference);
     }

    /**
     * Returns and initializes the classloader for this class.
     * @return the classloader
     */
    public final ClassLoader getClassLoader() {
        if (myClassLoader == null) {
            myClassLoader = (mClasspath == null)
                          // the usual classloader
                          ? getClass().getClassLoader()
                          // additional use the provided classpath
                          : new org.apache.tools.ant.AntClassLoader(
                        		  null, getProject(), mClasspath, true);
        }
        return myClassLoader;
    }

}
