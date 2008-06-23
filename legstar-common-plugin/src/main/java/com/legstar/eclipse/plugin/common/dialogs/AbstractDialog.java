package com.legstar.eclipse.plugin.common.dialogs;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.common.Activator;

/**
 * Groups methods useful to all dialogs.
 */
public abstract class AbstractDialog extends Dialog {
    
    /** The current plugin ID. */
    private String mPluginID;

    /**
     * @param parentShell the parent shell
     * @param pluginID the plugin ID
     */
    public AbstractDialog(final Shell parentShell, final String pluginID) {
        super(parentShell);
        mPluginID = pluginID;
    }

    /**
     * Create a label widget.
     * @param area parent composite
     * @param text label text
     * @return the new label
     */
    public static Label createLabel(final Composite area, final String text) {
        return createLabel(area, text, 1);
    }
    /**
     * Create a label widget.
     * @param area parent composite
     * @param text label text
     * @param span how many columns of the grid to span
     * @return the new label
     */
    public static Label createLabel(
    		final Composite area, final String text, final int span) {
        final Label label = new Label(area, SWT.NONE);
        label.setText(text);
        final GridData gridData = new GridData(GridData.VERTICAL_ALIGN_CENTER);
        gridData.horizontalSpan = span;
        label.setLayoutData(gridData);
        return label;
    }

    /**
     * Create a text field that fills a grid column.
     * @param container the parent container
     * @param initText the initial content
     * @param width the desired width (0 if none)
     * @return the new text field
     */
    public static Text createText(
    		final Composite container, final String initText, final int width) {
        return createText(container, initText, width, 1);
    }
    
    /**
     * Create a text field that fills several grid columns.
     * @param container the parent container
     * @param initText the initial content
     * @param width the desired width (0 if none, -1 to fill horizontally)
     * @param span how many columns of the grid this should span
     * @return the new text field
     */
    public static Text createText(
    		final Composite container,
    		final String initText,
    		final int width,
    		final int span) {
        final Text text = new Text(container, SWT.BORDER | SWT.SINGLE);
        final GridData gridData;
        if (width > -1) {
            gridData = new GridData();
            gridData.widthHint = width;
        } else {
            gridData = new GridData(GridData.FILL_HORIZONTAL);
        }
        gridData.horizontalSpan = span;
        text.setLayoutData(gridData);
        if (initText != null) {
            text.setText(initText);
        }
       return text;
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

    /**
     * Add a column to an SWT table.
     * @param table table to add column to
     * @param style an SWT style for the column
     * @param title the column header text
     * @param width the columns width
     * @return the newly created column
     */
    public static TableColumn createTableColumn(
            final Table table,
            final int style,
            final String title,
            final int width) {
        TableColumn tc = new TableColumn(table, style);
        tc.setText(title);
        tc.setResizable(true);
        if (width > -1) {
            tc.setWidth(width);
        } else {
            tc.pack();
        }
        return tc;
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
        return createTableColumn(table, style, title, -1);
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
        return createList(area, 1);
    }

    /**
     * Create a List widget.
     * @param area parent composite
     * @param span how many columns of the grid this should span
     * @return the new list widget
     */
    public static List createList(
    		final Composite area,
    		final int span) {
        List list = new List(area, SWT.READ_ONLY | SWT.SINGLE
                | SWT.BORDER | SWT.V_SCROLL);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = span;
        gd.heightHint = 100;
        list.setLayoutData(gd);
        return list;
    }

    /**
     * @return the current plugin ID
     */
    public final String getPluginID() {
        return mPluginID;
    }
    
    /**
     * Pops an error message.
     * @param dialogTitle title to appear on error dialog
     * @param message text
     */
    public void errorDialog(
            final String dialogTitle,
            final String message) { 
        IStatus status = new Status(
                IStatus.ERROR, Activator.PLUGIN_ID,
                IStatus.ERROR, message, null);         
        ErrorDialog.openError(getShell(), dialogTitle, null, status); 
    } 

    /**
     * Create a formatted core exception.
     * @param e an exception
     * @throws CoreException the core exception
     */
    public void throwCoreException(final Exception e) throws CoreException {
    	Activator.throwCoreException(e);
    }

    /**
     * Create a formatted core exception.
     * @param message the error message
     * @throws CoreException the core exception
     */
    public void throwCoreException(final String message) throws CoreException {
    	Activator.throwCoreException(message);
    }
}
