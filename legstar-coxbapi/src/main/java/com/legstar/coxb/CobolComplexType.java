/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 * Cobol annotation for JAXB Classes.  Used to keep track of the initial
 * java class when the Xml Schema was derived from a POJO.
 *
 */
@Retention(RetentionPolicy.RUNTIME)

//This annotation is associated with a class
@Target(ElementType.TYPE)

public @interface CobolComplexType {
    /** Initial Java class name from which an Xml Schema was derived leading
     * to this JAXB object. */
    String javaClassName();

}
