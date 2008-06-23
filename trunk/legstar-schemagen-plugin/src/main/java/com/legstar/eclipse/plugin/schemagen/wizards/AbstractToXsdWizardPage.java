package com.legstar.eclipse.plugin.schemagen.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;

/**
 * Abstract page. Collects parameters needed for common Schemagen
 * wizard pages. Each subclass will ad its own widgets. 
 *
 */
public abstract class AbstractToXsdWizardPage extends AbstractWizardPage {

	/**
	 * Construct the page.
	 * @param pageName the page name
	 * @param pageTitle the page title
	 * @param pageDesc the page description
	 * @param selection the current workbench selection
	 */
	protected AbstractToXsdWizardPage(
			final IStructuredSelection selection,
			final String pageName,
			final String pageTitle,
			final String pageDesc) {
		super(selection, pageName, pageTitle, pageDesc);
	}

	/**
     * Creates a combo box with the given items spanning all columns.
     * @param container the parent container
     * @param items  a list of items
     * @return the combo box 
     */
    protected Combo createComboFromItemsArray(
    		final Composite container, final String[] items) {
        final Combo combo = new Combo(container, SWT.READ_ONLY);
        combo.setItems(items);
        combo.select(0);
        combo.addSelectionListener(new SelectionListener() {

            /** {@inheritDoc}*/
            public void widgetDefaultSelected(final SelectionEvent arg0) {
                dialogChanged();
            }

            /** {@inheritDoc}*/
            public void widgetSelected(final SelectionEvent arg0) {
                dialogChanged();
            }
            
        });
            
        final GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.horizontalSpan = LAYOUT_COLUMNS;
        combo.setLayoutData(gridData);
        return combo;
    }
    
    /**
     * Create a multi line text field that fills a grid column.
     * @param container the parent container
     * @param span how many columns of the grid this should span
     * @return the new text field
     */
    protected Text createMultilineTextField(
    		final Composite container, final int span) {
        final Text text = new Text(container,
        		SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        final GridData gridData = new GridData(GridData.FILL_BOTH);
        gridData.horizontalSpan = span;
        text.setLayoutData(gridData);
        text.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
               dialogChanged();
            }
        });
        return text;
    }
    
}
