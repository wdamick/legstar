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
package com.legstar.coxb.util;

import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;

/**
 * Helper methods useful when manipulating bindings directly.
 *
 */
public final class BindingUtil {

    /** Utility class. */
    private BindingUtil() {

    }

    /**
     * Search for a binding with a specific name.
     * <p/>
     * This will descend the tree represented by bindings and their children until
     * it finds a binding with the requested name. 
     * @param rootBinding the starting point for the search
     * @param bindingName the 
     * @return the binding with the requested name or null if none is found
     * @throws HostException if no biending exist
     */
    public static ICobolBinding getBinding(
            final ICobolBinding rootBinding,
            final String bindingName) throws HostException {
        if (rootBinding.getBindingName().equals(bindingName)) {
            return rootBinding;
        }
        if (rootBinding instanceof ICobolComplexBinding) {
            for (ICobolBinding childBinding 
                    : ((ICobolComplexBinding) rootBinding).getChildrenList()) {
                ICobolBinding result = getBinding(childBinding, bindingName);
                if (result != null) {
                    return result;
                }
            }
        }
        if (rootBinding instanceof ICobolArrayComplexBinding) {
            return getBinding(
                    ((ICobolArrayComplexBinding) rootBinding).getComplexItemBinding(),
                    bindingName);
        }
        if (rootBinding instanceof ICobolChoiceBinding) {
            for (ICobolBinding childBinding 
                    : ((ICobolChoiceBinding) rootBinding).getAlternativesList()) {
                ICobolBinding result = getBinding(childBinding, bindingName);
                if (result != null) {
                    return result;
                }
            }
        }
        return null;

    }

}
