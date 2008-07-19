/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.schemagen.util;

import org.eclipse.jdt.core.IJavaProject;

/**
 * A simple data object to Java class names and projects. This is used in
 * conjunction with a list viewer and allows background processes to
 * create class name lists and associated class paths.
 */
public class JavaClass {
    
    /** The class name. */
	public String className;
	
    /** The Eclipse java project. */
    public IJavaProject javaProject;
    
    /**
     * Create a java class model.
     * @param name the class name
     * @param location the Eclipse java project
     */
    public JavaClass(final String name, final IJavaProject location) {
        this.className = name;
        this.javaProject = location;
    }
    
    /** {@inheritDoc} */
    public String toString() {
        return "Class name=" + className + ","
        	+ " Java project=" + javaProject.getProject().getName();
    }
    
    /**
     * Compares another object of this class to this instance.
     * @param jClass the other object
     * @return 0 if identical
     */
    public int compare(final JavaClass jClass) {
        int projectCompare =
        	this.javaProject.getProject().getName().compareToIgnoreCase(
                jClass.javaProject.getProject().getName());
        if (projectCompare == 0) {
            return this.className.compareToIgnoreCase(jClass.className);
        }
        return projectCompare;
    }
    
    /** {@inheritDoc} */
	@Override
	public boolean equals(final Object obj) {
		if (obj instanceof JavaClass) {
			return compare(((JavaClass) obj)) == 0 ? true : false;
		}
		return false;
	}

    /** {@inheritDoc} */
	@Override
	public int hashCode() {
		return super.hashCode();
	}

	/**
     * @return an instance of the referenced java class
     * @throws ClassNotFoundException if class cannot be found
     */
    public Class < ? > toClass() throws ClassNotFoundException {
        return Class.forName(className);
    }
    
}
