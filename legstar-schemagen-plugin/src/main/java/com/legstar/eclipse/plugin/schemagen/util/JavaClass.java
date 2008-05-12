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
        	+ " Java project=" + javaProject.getElementName();
    }
    
    /**
     * Compares another object of this class to this instance.
     * @param jClass the other object
     * @return 0 if identical
     */
    public int compare(final JavaClass jClass) {
        int locationCompare =
        	this.javaProject.getElementName().compareToIgnoreCase(
                jClass.javaProject.getElementName());
        if (locationCompare == 0) {
            return this.className.compareToIgnoreCase(jClass.className);
        }
        return locationCompare;
    }
    
    /**
     * @return an instance of the referenced java class
     * @throws ClassNotFoundException if class cannot be found
     */
    public Class < ? > toClass() throws ClassNotFoundException {
        return Class.forName(className);
    }
    
}
