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
package com.legstar.codegen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Map;
import java.util.Random;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.exception.MethodInvocationException;
import org.apache.velocity.exception.ParseErrorException;
import org.apache.velocity.exception.ResourceNotFoundException;

/**
 * Various utility methods which are mostly useful for code generation
 * using velocity templates.
 */
public final class CodeGenUtil {

    /** Generated code has reference to generation date following this format.*/
    public static final String DATE_FORMAT_NOW = "yyyy-MM-dd HH:mm:ss";
    
    /** Used to generate random serial version IDs. */
    private static Random mRandom = new Random();

    /** Logger. */
	private static final Log LOG = LogFactory.getLog(CodeGenUtil.class);
   
    /**
     * Defeats instantiation. Utility class.
     */
    private CodeGenUtil() {
     }
    
    /**
     * Check that a directory is valid.
     * 
     * @param dir
     *            the directory name to check
     * @param create
     *            true if directory should be created when not found
     */
    public static void checkDirectory(final String dir, final boolean create) {

        if (dir == null || dir.length() == 0) {
            throw (new IllegalArgumentException("No directory name was specified"));
        }

        File fdir = new File(dir);

        if (!fdir.exists()) {
            if (!create) {
                throw (new IllegalArgumentException(dir + " does not exist"));
            } else {
                if (!fdir.mkdirs()) {
                    throw (new IllegalArgumentException("Could not create directory "
                            + dir));
                } else {
                    return;
                }
            }
        }
        if (!fdir.isDirectory()) {
            throw (new IllegalArgumentException(dir + " is not a directory"));
        }
        if (!fdir.canWrite()) {
            throw (new IllegalArgumentException("Directory " + dir + " is not writable"));
        }
    }

    /**
     * Retrieve a file.
     * Given a directory name and a filename, this creates a File according to
     * the following rules:
     * <ul>
     *  <li>If the filename is absolute, the directory name is ignored</li>
     *  <li>If the directory is not null, it is assumed to exist</li>
     *  <li>If the directory is not null and the filename is not absolute, then
     *   directory is appended to the filename</li>
     * </ul>
     * @param dir
     *            parent directory
     * @param filename
     *            absolute or relative file name
     * @return a File
     */
    public static File getFile(final String dir, final String filename) {
        File file = new File(filename);
        if (file.isAbsolute()) {
            return file;
        }
        if (dir == null || dir.length() == 0) {
            return new File(filename);
        }
        return new File(dir, filename);
    }

    /**
     * Create a valid Java class name from a given noun.
     * 
     * @param noun
     *            the characters to turn into a java class name
     * @return the Java class name
     */
    public static String classNormalize(final String noun) {
        String className = null;
        if (noun != null && noun.length() > 0) {
            className = noun.substring(0, 1).toUpperCase();
            if (noun.length() > 1) {
                className += noun.substring(1, noun.length());
            }
        }
        return className;
    }

    /**
     * Given a package name, this method returns the relative path location of
     * the java files. A package like seg1.seg2.seg3 becomes /seg1/seg2/seg3/
     * 
     * @param packageName
     *            the package name
     * @return the relative location of java files
     */
    public static String relativeLocation(final String packageName) {
        if (packageName == null || packageName.length() == 0) {
            return "";
        }
        String loc = packageName.replace('.', '/');
        if (loc.charAt(0) != '/') {
            loc = '/' + loc;
        }
        if (loc.charAt(loc.length() - 1) != '/') {
            loc += '/';
        }
        return loc;
    }
    
    /**
     * Given a root directory name and a package name, returns the location
     * for class files making sure this location exist.
     * 
     * @param rootDirName the root directory name.
     * @param packageName the package name or null if none
     * @return an existing location to store class files
     */
    public static String classFilesLocation(
    		final String rootDirName, final  String packageName) {
		if (rootDirName == null || rootDirName.length() == 0) {
            throw (new IllegalArgumentException(
            		"No root directory name was specified"));
		}
    	String dir;
		if (packageName != null && packageName.length() > 0) {
			dir = rootDirName + '/'	+ CodeGenUtil.relativeLocation(packageName);
		} else {
			dir = rootDirName;
		}
		CodeGenUtil.checkDirectory(dir, true);
		return dir;
    }

    /**
     * Setup Velocity so that it searches for templates in the classpath.
     * @throws CodeGenVelocityException if setup fails
     */
    public static void initVelocity() throws CodeGenVelocityException {
        try {
			Velocity.addProperty("resource.loader", "classpath");
			Velocity.addProperty("classpath.resource.loader.description",
			        "Velocity Classpath Resource Loader");
			Velocity
			        .addProperty("classpath.resource.loader.class",
			                "org.apache.velocity.runtime.resource.loader."
			                + "ClasspathResourceLoader");
			Velocity.addProperty("classpath.resource.loader.cache", true);
			Velocity.init();
		} catch (Exception e) {
			throw new CodeGenVelocityException(e);
		}
    }

    /**
     * A simple context to use by generation templates.
     * @param the genrator name
     * @return a velocity context
     */
    public static VelocityContext getContext(final String generatorName) {
        VelocityContext context = new VelocityContext();
        context.put("formattedDate", now());
        context.put("generatorName", generatorName);
        return context;
    }

    /**
     * Apply a velocity template taken from a code generation make xml.
     * @param generatorName the generator name
     * @param templateName the velocity template to apply
     * @param modelName the model name
     * @param model the model providing data for velocity templates
     * @param parameters additional parameters to pass to template
     * @param targetFile the file to generate
     * @throws CodeGenMakeException if processing fails
     */
    public static void processTemplate(
    		final String generatorName,
            final String templateName,
            final String modelName,
            final Object model,
            final Map<String, Object> parameters,
            final File targetFile) throws CodeGenMakeException {
        
        if (LOG.isDebugEnabled()) {
            LOG.debug("Processing template");
            LOG.debug("Template name    = " + templateName);
            LOG.debug("Target file      = " + targetFile);
            if (parameters != null) {
	            for (String key : parameters.keySet()) {
	                Object value = parameters.get(key);
	                LOG.debug("Parameter " + key + " = " + value);
	            }
            }
        }
        VelocityContext context = CodeGenUtil.getContext(generatorName);
        context.put(modelName, model);
        context.put("serialVersionID", Long.toString(mRandom.nextLong()) + 'L');
        if (parameters != null) {
	        for (String key : parameters.keySet()) {
	            context.put(key, parameters.get(key));
	        }
        }
        StringWriter w = new StringWriter();

        try {
            Velocity.mergeTemplate(templateName, "UTF-8", context, w);
            BufferedWriter out = null;
            try {
                out = new BufferedWriter(new FileWriter(targetFile));
                out.write(w.toString());
            } catch (IOException e) {
                throw new CodeGenMakeException(e);
            } finally {
                if (out != null) {
                    out.close();
                }
            }
        } catch (ResourceNotFoundException e) {
            throw new CodeGenMakeException(e);
        } catch (ParseErrorException e) {
            throw new CodeGenMakeException(e);
        } catch (MethodInvocationException e) {
            throw new CodeGenMakeException(e);
        } catch (Exception e) {
            throw new CodeGenMakeException(e);
        }
    }
    
    /**
     * Formats todays date and time.
     * @return a formatted date
     */
    public static String now() {
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT_NOW);
        return sdf.format(cal.getTime());
    }

}
