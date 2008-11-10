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
package com.legstar.xsdc.test.cases.collections;

import java.util.ArrayList;
import java.util.List;

public class Container {
	
	private List < Item > mItemsList = new ArrayList < Item >() ;
	private Item[] mItemsArray = new Item[5];
	/**
	 * @return the mItemsList
	 */
	public final List<Item> getItemsList() {
		return mItemsList;
	}
	/**
	 * @param itemsList the mItemsList to set
	 */
	public final void setItemsList(List<Item> itemsList) {
		mItemsList = itemsList;
	}
	/**
	 * @return the mItemsArray
	 */
	public final Item[] getItemsArray() {
		return mItemsArray;
	}
	/**
	 * @param itemsArray the mItemsArray to set
	 */
	public final void setItemsArray(Item[] itemsArray) {
		mItemsArray = itemsArray;
	}
	

}
