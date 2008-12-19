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
 * public final class JavaToHostLsfileaeTransformer extends AbstractJavaToHostTransformer {
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
 * public final class JavaToHostLsfileaeTransformer extends AbstractJavaToHostTransformer {
 *      public ICobolComplexBinding getBinding() throws BindingException {
 *          return new com.legstar.test.coxb.lsfileae.DfhcommareaBinding();
 *      }
 *  }
 * </pre>
 */
public abstract class AbstractJavaToHostTransformer extends AbstractTransformer {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(AbstractJavaToHostTransformer.class);

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
        getCobolConverters().getCobolContext().setHostCharsetName(hostCharset);
        return transform(valueObject);
    }

    /**
     * Transforms java to host data.
     * @param valueObject a java value object
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Object valueObject) throws HostTransformException {

        long start = System.currentTimeMillis();
        if (LOG.isDebugEnabled()) {
            LOG.debug("Java to Host transformation started");
        }

        try {
            /* Request a binding from concrete class and attach the value object */
            ICobolComplexBinding binding = getBinding();
            binding.setObjectValue(valueObject);

            /* Allocate a byte array large enough to accommodate the largest object. */
            int size = binding.calcByteLength();
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

            if (LOG.isDebugEnabled()) {
                long end = System.currentTimeMillis();
                LOG.debug("Java to Host transformation ended. Processed: "
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
