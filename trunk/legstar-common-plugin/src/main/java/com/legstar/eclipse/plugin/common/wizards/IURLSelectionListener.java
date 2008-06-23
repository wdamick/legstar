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
