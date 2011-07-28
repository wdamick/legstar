

package com.legstar.coxb.cust.redsimpt;
import java.util.Hashtable;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.host.HostException;

import com.legstar.test.coxb.redsimpt.Dfhcommarea;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

    /** {@inheritDoc} */
    public ICobolBinding choose(
        final ICobolChoiceBinding choice,
        final Hashtable < String, Object > variablesMap,
        final CobolElementVisitor visitor) throws HostException {
        
        /* Get the parent value object which properties might help select the
         * right alternative. */
        Dfhcommarea valueObject = (Dfhcommarea) choice.getParentValueObject();
        assert (valueObject != null);
        
        /* Replace following code with actual logic. */
        int index = 0;
        switch (index) {
        case 0:
            return choice.getAlternativeByName("CDefinition1");
        case 1:
            return choice.getAlternativeByName("CDefinition2");
        case -1:
            /* An example of how to signal an exception.*/
            throw (new HostException("Unable to select an alternative"));
        default:
            /* Null will let the default choice strategy apply. */
            return null;
        }
    }
}


