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
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
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
    
    /** Suffix used for JAXB type variable names. */
    public static final String JAXB_TYPE_SUFFIX = "Type";
    
    /** Get the platform specific line separator.*/
    public static final String CRLF =
    	(String) java.security.AccessController.doPrivileged(
    		  new sun.security.action.GetPropertyAction("line.separator"));

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
     * @param errorDirName
     *            name to refer to if an error occurs
     */
    public static void checkDirectory(
    		final String dir, final boolean create, final String errorDirName) {
    	try {
			checkDirectory(dir, create);
		} catch (IllegalArgumentException e) {
			throw new IllegalArgumentException(
					errorDirName + ": " + e.getMessage());
		}
    }
    
    /**
     * Check that a directory is valid.
     * 
     * @param fdir
     *            the directory name to check
     * @param create
     *            true if directory should be created when not found
     * @param errorDirName
     *            name to refer to if an error occurs
     */
    public static void checkDirectory(
    		final File fdir, final boolean create, final String errorDirName) {
    	try {
			checkDirectory(fdir, create);
		} catch (IllegalArgumentException e) {
			throw new IllegalArgumentException(
					errorDirName + ": " + e.getMessage());
		}
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

        checkDirectory(new File(dir), create);
    }
    
    /**
     * Check that a directory is valid.
     * 
     * @param fdir the directory to check
     * @param create
     *            true if directory should be created when not found
     */
    public static void checkDirectory(final File fdir, final boolean create) {
    	
        if (fdir == null) {
            throw (new IllegalArgumentException("No directory name was specified"));
        }
        
        if (!fdir.exists()) {
            if (!create) {
                throw (new IllegalArgumentException(fdir.getName() + " does not exist"));
            } else {
                if (!fdir.mkdirs()) {
                    throw (new IllegalArgumentException("Could not create directory "
                            + fdir.getName()));
                } else {
                    return;
                }
            }
        }
        if (!fdir.isDirectory()) {
            throw (new IllegalArgumentException(fdir.getName()
            		+ " is not a directory"));
        }
        if (!fdir.canWrite()) {
            throw (new IllegalArgumentException("Directory "
            		+ fdir.getName() + " is not writable"));
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
     *   filename is appended to directory</li>
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
     * Retrieve a file.
     * Given a directory and a filename, this creates a File according to
     * the following rules:
     * <ul>
     *  <li>If the filename is absolute, the directory name is ignored</li>
     *  <li>Otherwise, filename is appended to directory</li>
     * </ul>
     * @param dir
     *            parent directory
     * @param filename
     *            absolute or relative file name
     * @return a File
     */
   public static File getFile(final File fdir, final String filename) {
        File file = new File(filename);
        if (file.isAbsolute()) {
            return file;
        }
        return new File(fdir, filename);
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
     * for class files. Optionally the location can be physically created.
     * 
     * @param rootDirName the root directory name.
     * @param packageName the package name or null if none
     * @param create
     *            true if directory should be created when not found
     * @return an existing location to store class files
     */
    public static String classFilesLocation(
    		final String rootDirName,
    		final  String packageName,
    		final boolean create) {
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
		if (create) {
			CodeGenUtil.checkDirectory(dir, true);
		}
		return dir;
    }
    
    /**
     * Concatenates the path derived from a package name to a root directory.
     * @param rootDir the root directory. Optionally the location can be
     *  physically created.
     * @param packageName the package name
     * @param create
     *            true if directory should be created when not found
     * @return the file derived from concatenating the root directory with the
     *  package path.
     */
    public static File classFilesLocation(
    		final File rootDir,
    		final  String packageName,
    		final boolean create) {
    	File dir = rootDir;
		if (packageName != null && packageName.length() > 0) {
			dir = new File(rootDir,  CodeGenUtil.relativeLocation(packageName));
		}
		if (create) {
			CodeGenUtil.checkDirectory(dir, true);
		}
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

    /**
     * Checks that a URI is valid and HTTP scheme.
     * @param URI the URI to check
     * @throws CodeGenMakeException
     */
    public static void checkHttpURI(
            final String httpUri) throws CodeGenMakeException {
        try {
            if (httpUri == null || httpUri.length() == 0) {
                throw new CodeGenMakeException(
                    "You must specify a valid URI");
            }
            URI uri = new URI(httpUri);
            if (uri.getScheme() == null ||
                    uri.getScheme().compareToIgnoreCase("http") != 0) {
                throw new CodeGenMakeException(
                        "URI " + uri + " must have http scheme");
            }
        } catch (URISyntaxException e) {
            throw new CodeGenMakeException(e);
        }
        
    }
    
    /**
     * Checks that a character set is valid
     * @param charset the character set
     * @see java.nio.charset.Charset
     * @throws CodeGenMakeException
     */
    public static void checkCharset(
            final String charset) throws CodeGenMakeException {
        if (charset == null || charset.length() == 0) {
            throw new CodeGenMakeException(
                    "You must specify a valid character set");
        }
        if (!Charset.isSupported(charset)) {
            throw new CodeGenMakeException(
                    "Character set " + charset + " is not supported");
        }
    }
    
    /**
     * Field names are derived from property names by lower casing the
     * first character.
     * @param propertyName the property name
     * @return a valid field name or null if property name is empty
     */
    public static String fieldNameFromPropertyName(final String propertyName) {
    	String fieldName = null;
    	if (propertyName != null && propertyName.length() > 0) {
    		fieldName = propertyName.substring(0, 1).toLowerCase();
    		if (propertyName.length() > 1) {
    			fieldName += propertyName.substring(1, propertyName.length());
    		}
    	}
    	return fieldName;
    }

    /**
     * Property names are derived from field names by upper casing the
     * first character.
     * @param fieldName the field name
     * @return a valid property name or null if field name is empty
     */
    public static String propertyNameFromFieldName(final String fieldName) {
    	String propertyName = null;
    	if (fieldName != null && fieldName.length() > 0) {
    		propertyName = fieldName.substring(0, 1).toUpperCase();
    		if (fieldName.length() > 1) {
    			propertyName += fieldName.substring(1, fieldName.length());
    		}
    	}
    	return propertyName;
    }

    /**
     * Property names are derived from jaxb type names by stripping the
     * type suffix (if any).
     * @param jaxbType the jaxb type name
     * @return a valid property name or null if jaxb type name is empty
     */
    public static String propertyNameFromJaxbType(final String jaxbType) {
    	String propertyName = null;
    	if (jaxbType != null && jaxbType.length() > 0) {
            propertyName = jaxbType;
            if (propertyName.endsWith(JAXB_TYPE_SUFFIX)) {
            	propertyName = propertyName.substring(0,
            			propertyName.length() - JAXB_TYPE_SUFFIX.length());
            }
    	}
    	return propertyName;
    }
}
