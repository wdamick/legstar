
package com.legstar.coxb.cust.dplarcht;
import java.util.Hashtable;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.host.HostException;
import com.legstar.test.coxb.dplarcht.LsItemsArrayType;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

    /** {@inheritDoc} */
    public final ICobolBinding choose(
        final ICobolChoiceBinding choice,
        final Hashtable < String, Object > variablesMap,
        final CobolElementVisitor visitor) throws HostException {
        
        /* Get the parent JAXB object which properties might help select the
         * right alternative. */
        LsItemsArrayType jaxbo = (LsItemsArrayType) choice.getObjectValue(LsItemsArrayType.class);
        assert (jaxbo != null);
        
        /* Replace following code with actual logic. */
        int index = 0;
        switch (index) {
        case 0:
            return choice.getAlternativeByName("lsFilesData");
        case 1:
            return choice.getAlternativeByName("lsProgramsData");
        case 2:
            return choice.getAlternativeByName("lsTransactionsData");
        case -1:
            /* An exemple of how to signal an exception.*/
            throw (new HostException("Unable to select an alternative"));
        default:
            /* Null will let the default choice strategy apply. */
            return null;
        }
    }
}
