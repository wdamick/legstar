/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.transform.AbstractXmlToHostTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * This implementation of an XML to host transformer dynamically binds to
 * a complex object using reflection.
 * The complex object must be a JAXB type with COBOL annotations.
 *
 */
public class ReflectXmlToHostTransformer extends AbstractXmlToHostTransformer {

    /**
     * Construct a transformer for a particular JAXB type.
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws ReflectBindingException if JAXB type has no annotations or cannot be 
     *  located from the classpath
     * @throws HostTransformException if transformer cannot be created
     */
    public ReflectXmlToHostTransformer(
            final String jaxbPackageName,
            final String jaxbType) throws ReflectBindingException, HostTransformException {
        super(new ReflectJavaToHostTransformer(jaxbPackageName, jaxbType));
    }

}
