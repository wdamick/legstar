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
package com.legstar.eclipse.plugin.schemagen.viewers;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

import com.legstar.eclipse.plugin.schemagen.util.JavaClass;

/**
 * Sorter for the TableViewer that displays items of type 
 * <code>JavaClass</code>.
 * Based on work from Mirasol http://www.opnworks.com
 * The sorter supports two sort criteria:
 * <p>
 * <code>JAVAPROJECT</code>: Java project name (String)
 * </p>
 * <p>
 * <code>JAVACLASS</code>: Java class name (String)
 * </p>
 */
public class JavaClassSorter extends ViewerSorter {

    /**
     * Constructor argument value that indicate to sort items by 
     * java project.
     */
    public static final int JAVAPROJECT  = 1;

    /**
     * Constructor argument value that indicate to sort items by 
     * java class name.
     */
    public static final int JAVACLASS = 2;

    /** Criteria that the instance uses. */ 
    private int mCriteria;

    /**
     * Creates a resource sorter that will use the given sort criteria.
     *
     * @param criteria the sort criterion to use: one of <code>NAME</code> or 
     *   <code>TYPE</code>
     */
    public JavaClassSorter(final int criteria) {
        super();
        mCriteria = criteria;
    }

    /**
     * {@inheritDoc}
     */
    public int compare(
            final Viewer viewer, final  Object o1, final  Object o2) {

        JavaClass javaClass1 = (JavaClass) o1;
        JavaClass javaClass2 = (JavaClass) o2;

        switch (mCriteria) {
        case JAVAPROJECT :
            return compareJavaProjects(javaClass1, javaClass2);
        case JAVACLASS :
            return compareClassNames(javaClass1, javaClass2);
        default:
            return 0;
        }
    }

    /**
     * Returns a number reflecting the collation order of the given java classes
     * based on the java project.
     *
     * @param javaClass1 the first javaClass element to be ordered
     * @param javaClass2 the second javaClass element to be ordered
     * @return a negative number if the first element is less  than the 
     *  second element; the value <code>0</code> if the first element is
     *  equal to the second element; and a positive number if the first
     *  element is greater than the second element
     */
    protected int compareJavaProjects(
            final JavaClass javaClass1, final JavaClass javaClass2) {
        return javaClass1.javaProject.getProject().getName().compareTo(
                javaClass2.javaProject.getProject().getName());
    }

    /**
     * Returns a number reflecting the collation order of the given java classes
     * based on their owner.
     *
     * @param javaClass1 the first resource element to be ordered
     * @param javaClass2 the second resource element to be ordered
     * @return a negative number if the first element is less  than the 
     *  second element; the value <code>0</code> if the first element is
     *  equal to the second element; and a positive number if the first
     *  element is greater than the second element
     */
    protected int compareClassNames(
            final JavaClass javaClass1, final JavaClass javaClass2) {
        return javaClass1.className.compareTo(javaClass2.className);
    }

    /**
     * Returns the sort criteria of this this sorter.
     *
     * @return the sort criterion
     */
    public int getCriteria() {
        return mCriteria;
    }
}
