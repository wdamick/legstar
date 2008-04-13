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
