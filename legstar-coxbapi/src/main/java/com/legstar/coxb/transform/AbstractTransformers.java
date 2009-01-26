/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
 * A generic class that provides transformer capabilities for a given structure.
 * <p/>
 * A structure maps to a java class and a COBOL structure.
 * This class does not implement the transformers, it acts as a container.
 * <p/>
 * Classes derived from this one will typically implement a constructor that
 * creates the directional transformers, java to host and host to java.
 *
 */
public abstract class AbstractTransformers implements IHostTransformers {

    /** Transformer that turns a java data object into host data. */
    private IJavaToHostTransformer mJavaToHost;

    /** Transformer that turns host data into a java data object. */
    private IHostToJavaTransformer mHostToJava;
    
    /**
     * No arg constructor. Caller is responsible for setting the internal transformers.
     */
    public AbstractTransformers() {
        
    }
    
    /**
     * Creates a provider with its directional transformers.
     * @param javaToHost java to host transformer
     * @param hostToJava host to java transformer
     */
    public AbstractTransformers(
            final IJavaToHostTransformer javaToHost,
            final IHostToJavaTransformer hostToJava) {
        mJavaToHost = javaToHost;
        mHostToJava = hostToJava;
    }
 
    /**
     * @return the transformer that turns a java data object into host data
     */
    public IJavaToHostTransformer getJavaToHost() {
        return mJavaToHost;
    }

    /**
     * @param javaToHost the the transformer that turns a java data object into host data to set
     */
    public void setJavaToHost(
            final IJavaToHostTransformer javaToHost) {
        mJavaToHost = javaToHost;
    }

    /**
     * @return the transformer that turns host data into a java data object
     */
    public IHostToJavaTransformer getHostToJava() {
        return mHostToJava;
    }

     /**
     * @param hostToJava the transformer that turns host data into a java data object to set
     */
    public void setHostToJava(
            final IHostToJavaTransformer hostToJava) {
        mHostToJava = hostToJava;
    }

}
