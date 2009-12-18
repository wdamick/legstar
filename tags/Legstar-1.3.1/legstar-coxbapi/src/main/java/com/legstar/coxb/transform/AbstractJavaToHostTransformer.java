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
 * Generic methods to transform java value object to host data.
 * <p/>
 * Implementing classes should inherit from AbstractJavaToHostTransformer and implement
 * the getBinding method.
 * <p/>
 * This is sample code with dynamic binding:
 * <pre>
 * public class JavaToHostLsfileaeTransformer extends AbstractJavaToHostTransformer {
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
 * public class JavaToHostLsfileaeTransformer extends AbstractJavaToHostTransformer {
 *      public ICobolComplexBinding getBinding() throws BindingException {
 *          return new com.legstar.test.coxb.lsfileae.DfhcommareaBinding();
 *      }
 *  }
 * </pre>
 */
public abstract class AbstractJavaToHostTransformer extends AbstractTransformer implements IJavaToHostTransformer {

    /** Logger. */
    private final Log _log = LogFactory.getLog(AbstractJavaToHostTransformer.class);

    /**
     * Create a Java to Host transformer using default COBOL parameters.
     */
    public AbstractJavaToHostTransformer() {
        super();
    }

    /**
     * Create a Java to Host transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     */
    public AbstractJavaToHostTransformer(final String hostCharset) {
        super(hostCharset);
    }

    /**
     * Create a Java to Host transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public AbstractJavaToHostTransformer(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /**
     * Transforms java to host data with a specific host character set.
     * @param valueObject a java value object
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Object valueObject, final String hostCharset) throws HostTransformException {
        if (hostCharset != null && hostCharset.length() > 0) {
            getCobolConverters().getCobolContext().setHostCharsetName(hostCharset);
        }
        return transform(valueObject);
    }

    /**
     * Transforms java data object to host data.
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Object valueObject) throws HostTransformException {

        long start = System.currentTimeMillis();
        if (_log.isDebugEnabled()) {
            _log.debug("Java to Host transformation started");
        }

        try {
            /* Reuse binding if possible get a new one otherwise */
            ICobolComplexBinding binding = getCachedBinding();
            binding.setObjectValue(valueObject);

            /* Allocate a byte array large enough to accommodate the largest object. */
            int size = binding.getByteLength();
            byte[] hostData = new byte[size];

            /* create the outbound buffer by marshalling the java object tree */
            CobolElementVisitor marshaler = getCobolBindingVisitorsFactory().createMarshalVisitor(
                hostData, 0, getCobolConverters());

            /* Traverse the object structure, visiting each node with the visitor */
            binding.accept(marshaler);

            /* Get the actual bytes marshalled */
            int bytesMarshalled = marshaler.getOffset();

            /* If the byte array was allocated too large (this happens with
             * variable size arrays for instance), reallocate. */
            byte[] adjustedHostData;
            if (bytesMarshalled < size) {
                adjustedHostData = new byte[bytesMarshalled];
                System.arraycopy(hostData, 0, adjustedHostData, 0, bytesMarshalled);
            } else {
                adjustedHostData = hostData;
            }

            if (_log.isDebugEnabled()) {
                long end = System.currentTimeMillis();
                _log.debug("Java to Host transformation ended. Processed: "
                        + Integer.toString(bytesMarshalled) + " bytes "
                        + "elapse:"
                        + Long.toString(end - start) + " ms");
            }

            return adjustedHostData;

        } catch (HostException he) {
            throw new HostTransformException(he);
        }
    }

 }
