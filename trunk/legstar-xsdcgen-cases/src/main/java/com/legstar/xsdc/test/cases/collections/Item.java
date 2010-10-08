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
package com.legstar.xsdc.test.cases.collections;

/**
 * Collections item.
 *
 */
public class Item {

    /** A String sub-Item. */
    private String mStringItem;
    
    /** An int sub-item. */
    private int mIntItem;
    /**
     * @return the mStringItem
     */
    public String getStringItem() {
        return mStringItem;
    }
    /**
     * @param stringItem the mStringItem to set
     */
    public void setStringItem(final String stringItem) {
        mStringItem = stringItem;
    }
    /**
     * @return the mIntItem
     */
    public int getIntItem() {
        return mIntItem;
    }
    /**
     * @param intItem the mIntItem to set
     */
    public void setIntItem(final int intItem) {
        mIntItem = intItem;
    }

}
