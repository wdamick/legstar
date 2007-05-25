/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.eclipse.plugin.cixsgen.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;

/**
 * General purpose SWT helper class. Helps ensure all widgets are consistently
 * laid out on all dialogs.
 *
 */
public final class WidgetCreator {
	
	/**
	 * Utility class exports static methods only.
	 */
	private WidgetCreator() {
		
	}
	/**
	 * Create a label widget.
	 * @param area parent composite
	 * @param text label text
	 * @return the new label
	 */
	public static Label createLabel(final Composite area, final String text) {
		Label label = new Label(area, SWT.NONE);
		label.setText(text);
		label.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER));
		return label;
	}

	/**
	 * Create a Text widget.
	 * @param area parent composite
	 * @param initText the initial content
	 * @return the new text widget
	 */
	public static Text createText(final Composite area, final String initText) {
		Text text = new Text(area, SWT.BORDER);
		GridData gdRight = new GridData(GridData.FILL_HORIZONTAL);
		gdRight.widthHint = 200;
		text.setLayoutData(gdRight);
		if (initText != null) {
			text.setText(initText);
		} else {
			text.setText("");
		}
		return text;
	}

	/**
	 * Create a Combo widget.
	 * @param area parent composite
	 * @return the new combo widget
	 */
	public static Combo createCombo(final Composite area) {
		Combo combo = new Combo(area, SWT.READ_ONLY | SWT.SINGLE
				| SWT.BORDER | SWT.V_SCROLL);
		combo.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		return combo;
	}
	
	/**
	 * Create a List widget.
	 * @param area parent composite
	 * @return the new list widget
	 */
	public static List createList(final Composite area) {
		List list = new List(area, SWT.READ_ONLY | SWT.SINGLE
				| SWT.BORDER | SWT.V_SCROLL);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.widthHint = 200;
		gd.heightHint = 100;
		list.setLayoutData(gd);
		return list;
	}
	
	/**
	 * Add a column to an SWT table.
	 * @param table table to add column to
	 * @param style an SWT style for the column
	 * @param title the column header text
	 * @return the newly created column
	 */
	public static TableColumn createTableColumn(
			final Table table,
			final int style,
			final String title) {
	    TableColumn tc = new TableColumn(table, style);
	    tc.setText(title);
	    tc.setResizable(true);
	    tc.pack();
	    return tc;
	}
	
	/**
	 * Add a new button on a composite. Is in disabled state initially.
	 * @param parent the parent composite
	 * @param text text to appear on button
	 * @return the newly created button
	 */
	public static Button createButton(
			final Composite parent, final String text) {
		Button button = new Button(parent, SWT.PUSH);
		button.setText(text);
		button.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		button.setEnabled(false);
		return button;
	}
	

}
