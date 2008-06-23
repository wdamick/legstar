package com.legstar.eclipse.plugin.common.preferences;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;

/**
 * A simple class to store a limited number of historical URLs.
 * Since typing URLs is painful it helps to store new ones typed and
 * be able to retrieve that across sessions.
 *
 */
public class PreferenceUrlHistory {
	
	/** We are not going to store more than this amount of historical data. */
	public static final int HISTORY_SIZE = 10;
	
	/** The preference store used as history storage.*/
	private IPreferenceStore mStore;
	
	/** Identifies entries in store.*/
	private String mKeyPrefix;
	
	/**
	 * @param store preference store used as history storage
	 * @param keyPrefix used with a numeric index to identify entries in
	 * store, oldest being number 0
	 */
	public PreferenceUrlHistory(
			final IPreferenceStore store, final String keyPrefix) {
		mStore = store;
		mKeyPrefix = keyPrefix;
	}
	
	/**
	 * This is a best effort type of process. If the text already exists, the
	 * method returns silently. If the maximum size of the history is reached
	 * the oldest is pushed out.
	 * @param value the new value to add.
	 */
	public void add(final String value) {
		List < String > history = get();
		if (history.contains(value)) {
			return;
		}
		if (history.size() >= HISTORY_SIZE) {
			history.remove(0);
		}
		history.add(value);
		set(history);
	}
	
	/**
	 * Retrieves the current history. Oldest is first in the list.
	 * @return the current history list. Might be empty.
	 */
	public List < String > get() {
		List < String > list = new ArrayList < String >();
		for (int i = 0; i < HISTORY_SIZE; i++) {
			String value = mStore.getString(mKeyPrefix + i);
			if (value.length() == 0) {
				break;
			}
			list.add(value);
		}
		return list;
	}
	
	/**
	 * Stores a history list in storage.
	 * @param history a history list to replace existing one
	 */
	public void set(final List < String > history) {
		int i = 0;
		for (String value : history) {
			mStore.setValue(mKeyPrefix + i, value);
			i++;
		}
	}

}
