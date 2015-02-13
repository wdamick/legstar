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
package com.legstar.coxb.transform;

/**
 * A transformers class where individual transformers use reflection
 * on JAXB classes to perform the actual binding.
 *
 */
public class DplarchtTransformers extends AbstractTransformers {

    
    /**
     * Creates a transformer for each direction at construction time.
     */
    public DplarchtTransformers() {
        super(new JavaToHostDplarchtTransformer(),
                new HostToJavaDplarchtTransformer());
    }

    /** {@inheritDoc}*/
    public byte[] toHost(final Object valueObject, final String hostCharset)
            throws HostTransformException {
        return getJavaToHost().transform(valueObject, hostCharset);
    }

    /** {@inheritDoc}*/
    public byte[] toHost(final Object valueObject) throws HostTransformException {
        return getJavaToHost().transform(valueObject);
    }

    /** {@inheritDoc}*/
    public Object toJava(final byte[] hostData, final String hostCharset)
            throws HostTransformException {
        return getHostToJava().transform(hostData, hostCharset);
    }

    /** {@inheritDoc}*/
    public Object toJava(final byte[] hostData) throws HostTransformException {
        return getHostToJava().transform(hostData);
    }

}
