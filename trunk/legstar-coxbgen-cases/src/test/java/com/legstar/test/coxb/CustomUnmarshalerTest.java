package com.legstar.test.coxb;

import java.util.Hashtable;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.coxb.util.BindingUtil;
import com.legstar.test.coxb.lsfileaq.Dfhcommarea;
import com.legstar.test.coxb.lsfileaq.bind.DfhcommareaBinding;
import com.legstar.test.coxb.lsfileaq.bind.DfhcommareaHostToJavaTransformer;

import junit.framework.TestCase;

/**
 * Test the capability to use a custom unmarshaler and act on alternative choices.
 * This is useful as an alternative to setting things like unmarshalChoiceStrategyClassName
 * in the originating XML Schema and COXB annotations.
 *
 */
public class CustomUnmarshalerTest extends TestCase {

    /**
     * Shows how a custom unmarshaler can repace the unmarshalChoiceStrategyClassName
     * and customVariable annotations.
     * @throws HostException if test fails
     */
    public void testCustomUnmarshaler() throws HostException {
        CobolElementVisitor unmarshaler =
            new CustomCobolUnmarshalVisitor(
                    HostData.toByteArray(LsfileaqCases.getHostBytesHexRequestReply5()),
                    0,
                    new CobolSimpleConverters(new CobolContext()));
        /* Request a binding from concrete class */
        ICobolComplexBinding binding = new DfhcommareaBinding();

        /* Traverse the object structure, visiting each node with the visitor */
        binding.accept(unmarshaler);

        /* Get the actual bytes unmarshalled */
        int bytesUnmarshalled = unmarshaler.getOffset();
        assertTrue(bytesUnmarshalled > 0);

        /* Check the java object */
        Dfhcommarea commarea = (Dfhcommarea) binding.getObjectValue(Dfhcommarea.class);
        LsfileaqCases.checkJavaObjectReplyAlt5(commarea);
    }

    /**
     * In this case we dynamically set an unmarshalChoiceStrategy which is not
     * part of annotations.
     * @throws HostException if getting the binding fails
     * @throws HostTransformException if unmarshaling fails
     */
    public void testDynamicUnmarshalChoiceStrategy() throws HostException, HostTransformException {
        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();

        /* find the choice binding */
        ICobolChoiceBinding lastTransDateChoiceBinding =
            (ICobolChoiceBinding) BindingUtil.getBinding(transformer.getCachedBinding(),
            "LastTransDateChoice");
        assertNotNull(lastTransDateChoiceBinding);

        /* set the unmarshal strategy */
        lastTransDateChoiceBinding.setUnmarshalChoiceStrategy(
                new UnmarshalChoiceStrategy());

        /* fire the transformation */
        Dfhcommarea commarea = transformer.transform(
                HostData.toByteArray(LsfileaqCases.getHostBytesHexRequestReply5()));
        LsfileaqCases.checkJavaObjectReplyAlt5(commarea);


    }

    /**
     * A sample custom unmarshaller.
     *
     */
    public class CustomCobolUnmarshalVisitor extends CobolUnmarshalVisitor {

        /**
         * Create an instance.
         * @param hostBytes the mainframe payload
         * @param offset where to start
         * @param cobolConverters what converters to use
         */
        public CustomCobolUnmarshalVisitor(final byte[] hostBytes, final int offset,
                final ICobolConverters cobolConverters) {
            super(hostBytes, offset, cobolConverters);
        }
        /** {@inheritDoc}*/
        public void visit(final ICobolChoiceBinding ce)
        throws HostException {
            for (ICobolBinding alt : ce.getAlternativesList()) {
                if (!alt.getCobolName().equals("LAST-TRANS-DATE")) {
                    alt.accept(this);
                    ce.setPropertyValue(
                            ce.getAlternativesList().indexOf(alt));
                }
            }
        }
    }

    /**
     * A sample on the fly unmarshalChoiceStrategy.
     *
     */
    public class UnmarshalChoiceStrategy  implements ICobolUnmarshalChoiceStrategy {

        /** {@inheritDoc}*/
        public ICobolBinding choose(final ICobolChoiceBinding choice,
                final Hashtable < String, Object > variablesMap,
                final CobolElementVisitor visitor) throws HostException {
            return choice.getAlternativeByName("Filler49");
        }

    }
}
