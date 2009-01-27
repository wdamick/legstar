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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;

/**
 * Generic methods to transform host data to java.
 * <p/>
 * Implementing classes should inherit from AbstractHostToJavaTransformer and implement
 * the getBinding method.
 * <p/>
 * This is sample code with dynamic binding:
 * <pre>
 * public final class HostToJavaLsfileaeTransformer extends AbstractHostToJavaTransformer {
 *      public ICobolComplexBinding getBinding() throws BindingException {
 *          try {
 *              CComplexReflectBinding binding = new CComplexReflectBinding(
 *                      new com.legstar.test.coxb.lsfileae.ObjectFactory(),
 *                      com.legstar.test.coxb.lsfileae.Dfhcommarea.class);
 *              return binding;
 *          } catch (ReflectBindingException e) {
 *              throw new BindingException(e);
 *          }
 *      }
 *  }
 * </pre>
 * <p/>
 * This is sample code with static binding:
 * <pre>
 * public final class HostToJavaLsfileaeTransformer extends AbstractHostToJavaTransformer {
 *      public ICobolComplexBinding getBinding() throws BindingException {
 *          return new com.legstar.test.coxb.lsfileae.DfhcommareaBinding();
 *      }
 *  }
 * </pre>
 */
public abstract class AbstractHostToJavaTransformer extends AbstractTransformer implements IHostToJavaTransformer {
    
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(AbstractHostToJavaTransformer.class);

    /**
     * Create a Host to Java transformer using default COBOL parameters.
     */
    public AbstractHostToJavaTransformer() {
        super();
    }
    
    /**
     * Create a Host to Java transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     */
    public AbstractHostToJavaTransformer(final String hostCharset) {
        super(hostCharset);
    }
    
    /**
     * Create a Host to Java transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public AbstractHostToJavaTransformer(final CobolContext cobolContext) {
        super(cobolContext);
    }
    
    /**
     * Transforms host data to java data object with a specific host character set.
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @param hostCharset the host character set
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    @SuppressWarnings("unchecked")
    public < T > T  transform(final byte[] hostData, final String hostCharset) throws HostTransformException {
        if (hostCharset != null && hostCharset.length() > 0) {
            getCobolConverters().getCobolContext().setHostCharsetName(hostCharset);
        }
        return (T) transform(hostData);
    }
    
    /**
     * Transforms host data to java data object.
     * @param <T> the bound object type
     * @param hostData a byte array containing host data
     * @return a Java value object
     * @throws HostTransformException if transformation fails
     */
    @SuppressWarnings("unchecked")
    public < T > T transform(final byte[] hostData) throws HostTransformException {

        long start = System.currentTimeMillis();
        if (LOG.isDebugEnabled()) {
            LOG.debug("Host to Java transformation started");
        }

        try {
            /* Unmarshal the raw host data into a java object tree */
            CobolElementVisitor unmarshaler = getCobolBindingVisitorsFactory().createUnmarshalVisitor(
                    hostData, 0, getCobolConverters());

            /* Request a binding from concrete class */
            ICobolComplexBinding binding = getBinding();
            
            /* Traverse the object structure, visiting each node with the visitor */
            binding.accept(unmarshaler);
            
            /* Get the actual bytes unmarshalled */
            int bytesUnmarshalled = unmarshaler.getOffset();

            if (LOG.isDebugEnabled()) {
                long end = System.currentTimeMillis();
                LOG.debug("Host to Java transformation ended Processed: "
                        + Integer.toString(bytesUnmarshalled) + " bytes "
                        + "elapse:"
                        + Long.toString(end - start) + " ms");
            }
            
            return (T) binding.getObjectValue((Class < T >) binding.getJaxbType());
            
        } catch (HostException he) {
            throw new HostTransformException(he);
        }
    }

}
