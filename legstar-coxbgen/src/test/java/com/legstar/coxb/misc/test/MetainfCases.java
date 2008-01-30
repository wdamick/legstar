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
package com.legstar.coxb.misc.test;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.Commandline;

public class MetainfCases extends Task {

    public final List<URL> classpaths = new ArrayList<URL>();
    /** Additional command line arguments. */
    private final Commandline cmdLine = new Commandline();

    public final void execute() {
        System.out.println("execute");
    	String serviceId = "META-INF/services/" + "com.sun.tools.xjc.Plugin";
        try {
			parseArguments(cmdLine.getArguments());
		} catch (Exception e1) {
			e1.printStackTrace();
			throw new BuildException(e1.getMessage());
		}
        
        // used to avoid creating the same instance twice
       Set<String> classNames = new HashSet<String>();
       
       ClassLoader classLoader = getUserClassLoader(getClass().getClassLoader());
        try {
			Enumeration<URL> e = classLoader.getResources(serviceId);
			if (e == null) {
				System.out.println("no " + serviceId);
			} else {
	            while(e.hasMoreElements()) {
	                URL url = e.nextElement();
					System.out.println("url=" + url.toString());
	                BufferedReader reader=null;
                    reader = new BufferedReader(new InputStreamReader(url.openStream()));
                    String impl;
                    while((impl = reader.readLine())!=null ) {
                        // try to instanciate the object
                        impl = impl.trim();
                        if(classNames.add(impl)) {
                            System.out.println("Attempting to load "+impl);
                            Class < ? > implClass = classLoader.loadClass(impl);
                            System.out.println("Attempting to instanciate "+impl);
                            implClass.newInstance();
                        }
                    }
                    reader.close();
                }
            }
		} catch (IOException e) {
			e.printStackTrace();
			throw new BuildException("IOException" + e.getMessage());
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			throw new BuildException("ClassNotFoundException " + e.getMessage());
		} catch (InstantiationException e) {
			e.printStackTrace();
			throw new BuildException("InstantiationException " + e.getMessage());
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw new BuildException("IllegalAccessException " + e.getMessage());
		} catch (NoClassDefFoundError e) {
			e.printStackTrace();
			throw new BuildException("Exception " + e.getMessage());
		}


    }

    public void parseArguments( String[] args ) throws Exception {
       System.out.println("parseArguments");
       for (int i = 0; i < args.length; i++) {
           System.out.println("arg" + i + " =" + args[i]);
            if(args[i].length()==0)
                throw new Exception();
            if (args[i].charAt(0) == '-') {
                parseArgument(args,i);
            }
        }
    }


    protected int parseArgument( String[] args, int i ) throws Exception {
       System.out.println("parseArgument");
       if (args[i].equals("-classpath") || args[i].equals("-cp")) {
            if (i == args.length - 1)
                throw (new Exception("MISSING_CLASSPATH"));
            File file = new File(args[++i]);
            try {
                classpaths.add(file.toURI().toURL());
                System.out.println(file.toURI().toURL().toString() + " added to classpath");
            } catch (MalformedURLException e) {
                throw (new Exception("NOT_A_VALID_FILENAME" + file.toString()));
            }
            return 2;
        }
        
        return 0;   // unrecognized
    }
    
    /**
     * Gets a classLoader that can load classes specified via the
     * -classpath option.
     */
    public URLClassLoader getUserClassLoader( ClassLoader parent ) {
        System.out.println("getUserClassLoader");
        return new URLClassLoader(
                classpaths.toArray(new URL[classpaths.size()]),parent);
    }

    public Commandline.Argument createArg() {
        System.out.println("createArg");
        return cmdLine.createArgument();
    }
}
