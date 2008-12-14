/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.common.wizards;

/**
 * Implementing classes will typically perform some processing based on a
 * particular URL being selected.
 *
 */
public interface IURLSelectionListener {

    /**
     * A URL was selected.
     * @param urlString the selected URL
     */
    void urlSelected(final String urlString);

}
