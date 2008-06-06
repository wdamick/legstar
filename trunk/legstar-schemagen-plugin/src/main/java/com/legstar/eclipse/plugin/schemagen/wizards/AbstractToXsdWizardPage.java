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
package com.legstar.eclipse.plugin.schemagen.wizards;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.eclipse.ui.forms.events.IHyperlinkListener;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;


/**
 * A generic wizard page that gives all LegStar wizards the same look
 * and feel.
 */
public abstract class AbstractToXsdWizardPage extends WizardPage {

    /** The initial selection in the workspace. */
    private IStructuredSelection mInitialSelection = null;
    
    /** The main grid layout column number. */
    public static final int LAYOUT_COLUMNS = 3;

    /** Browse button label text. */
    private static final String BROWSE_LABEL = "Browse...";
    
	/**
     * Construct a wizard page.
     * @param initialSelection the workbench current selection
     * @param pageName the name of this page
     * @param title the title that should appear on this page
     * @param description the text that should describe the purpose of this page
     */
    protected AbstractToXsdWizardPage(
            final IStructuredSelection initialSelection,
            final String pageName,
            final String title,
            final String description) {
        super(pageName);
        setTitle(title);
        setDescription(description);
        mInitialSelection = initialSelection;
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(
                		com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID,
                		com.legstar.eclipse.plugin.common.Activator.LOGO_IMG);
        setImageDescriptor(image);
    }

    /** {@inheritDoc}*/
    public void createControl(final Composite parent) {
        Composite container = new Composite(parent, SWT.NULL);
        final GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = LAYOUT_COLUMNS;
        container.setLayout(gridLayout);
        setControl(container);
        
        createExtendedControls(container);
        
        initContents();
        dialogChanged();
    }

    /**
     * Set the initial values for controls on this page.
     */
    protected abstract void initContents();

    /**
     * Add the controls that are specific to this wizard page.
     * @param container the parent container
     */
    protected abstract void createExtendedControls(final Composite container);
    
    /**
     * Process new input from user. Validate all control contents.
     */
    protected abstract void dialogChanged();
    
    /**
     * Creates a combo box with the given items spanning all columns.
     * @param container the parent container
     * @param items  a list of items
     * @return the combo box 
     */
    protected Combo createCombo(
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
     * Create a text field that fills a grid column.
     * @param container the parent container
     * @return the new text field
     */
    protected Text createTextField(final Composite container) {
        return createTextField(container, 1);
    }
    
    /**
     * Create a text field that fills a grid column.
     * @param container the parent container
     * @param span how many columns of the grid this should span
     * @return the new text field
     */
    protected Text createTextField(final Composite container, final int span) {
        final Text text = new Text(container, SWT.BORDER | SWT.SINGLE);
        final GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.horizontalSpan = span;
        text.setLayoutData(gridData);
        text.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
               dialogChanged();
            }
        });
        return text;
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
    
    /**
     * Create a browse button. It pops up a dialog to select a folder and
     * sets an associated text field with the result.
     * @param container parent container
     * @param dialogTitle title that browse dialog should display
     * @param result Text field to update on return from dialog
     * @return a new button
     */
    protected Button createBrowseForContainerButton(
            final Composite container,
            final String dialogTitle,
            final Text result) {
        final Button button = new Button(container, SWT.PUSH);
        button.setText(BROWSE_LABEL);
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                Path destPath = handleBrowseForContainer(dialogTitle);
                if (destPath == null) {
                    result.setText("");
                } else {
                    result.setText(destPath.toOSString());
                }
            }
        });
        return button;
    }
    
    /**
     * Create a browse button. It pops up a dialog to select files. It
     * then reads each file and store its content in a text area.
     * @param container parent container
     * @param dialogTitle title that browse dialog should display
     * @param result Text field to update on return from dialog
     * @return a new button
     */
    protected Button createBrowseForFileContentButton(
            final Composite container,
            final String dialogTitle,
            final Text result) {
        final Button button = new Button(container, SWT.PUSH);
        button.setText(BROWSE_LABEL);
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                result.setText(selectSingleFileContent(dialogTitle));
            }
        });
        return button;
    }
    
    /**
     * Opens a file selection dialog and merges all selected files
     * contents into a single string. 
     * @param dialogTitle title that browse dialog should display
     * @return a cumulative content of all files selected
     */
    public String selectSingleFileContent(final String dialogTitle) {
        String fileName = handleBrowseForFiles(dialogTitle);
        StringBuilder sb = new StringBuilder();
        if (fileName != null) {
            try {
                sb.append(getContent(fileName));
            } catch (IOException e1) {
                sb.append("Error opening " + fileName
                        + " " + e1.getMessage());
            }
        }
        return sb.toString();
    }
    
    /**
     * Reads the content of a file in a string.
     * @param fileName name of the file
     * @return a string with the file content
     * @throws IOException if fails to read file
     */
    private static String getContent(
            final String fileName) throws IOException {
        BufferedReader in = new BufferedReader(new FileReader(fileName));
        StringBuilder resStr = new StringBuilder();
        String str = in.readLine();
        while (str != null) {
            resStr.append(str + '\n');
            str = in.readLine();
        }
        in.close();
        return resStr.toString();
    }
    
    /**
     * Adds a label that spans only one column.
     * @param container parent composite
     * @param text the label text
     * @return the new label
     */
    protected Label createLabel(
    		final Composite container, final String text) {
        return createLabel(container, text, 1);
    }

    /**
     * Adds a label that spans only one column.
     * @param container parent composite
     * @param text the label text
     * @param span how many columns of the grid this should span
     * @return the new label
     */
    protected Label createLabel(
    		final Composite container, final String text, final int span) {
        final Label label = new Label(container, SWT.NONE);
        final GridData gridData = new GridData();
        gridData.horizontalSpan = span;
        label.setLayoutData(gridData);
        label.setText(text);
        return label;
    }

    /**
     * Adds a a group that spans all columns.
     * @param container parent composite
     * @param text the group text
     * @return the new group
     */
    protected Group createGroup(
    		final Composite container, final String text) {
        final Group group = new Group(container, SWT.SHADOW_ETCHED_IN);
        final GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.horizontalSpan = LAYOUT_COLUMNS;
        group.setLayoutData(gridData);
        group.setText(text);
        final GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = LAYOUT_COLUMNS;
        group.setLayout(gridLayout);
        return group;
    }
    
    /**
     * Create a Hyperlink Eclipse 3.0 style.
     * @param container the parent container
     * @param text the text of the hyperlink
     * @param image an image to show on the left of the ling
     * @param listener what to do when link is clicked
     * @return the new hyperlink
     */
    protected ImageHyperlink createHyperlink(
            final Composite container,
            final String text,
            final Image image,
            final IHyperlinkListener listener) {
        ImageHyperlink link = new ImageHyperlink(container, SWT.NULL);
        link.setText(text);
        link.setImage(image);
        link.setUnderlined(true);
        link.addHyperlinkListener(listener);
        final GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.horizontalSpan = LAYOUT_COLUMNS;
        link.setLayoutData(gridData);
        return link;
    }
    
    /**
     * Uses the standard container selection dialog to choose the new value for
     * a container field.
     * @param dialogTitle what the title should say
     * @return a Path to the selected location or null
     */
    private Path handleBrowseForContainer(final String dialogTitle) {
        ContainerSelectionDialog dialog = new ContainerSelectionDialog(
                getShell(), null, true, dialogTitle);
        if (dialog.open() == ContainerSelectionDialog.OK) {
            Object[] result = dialog.getResult();
            if (result.length == 1) {
                return (Path) result[0];
            }
        }
        return null;
    }
    
    /**
     * Uses the standard file selection dialog to choose a set of files
     * from the file system.
     * @param dialogTitle what the title should say
     * @return an array of selected file names or null
     */
    private String handleBrowseForFiles(final String dialogTitle) {
        FileDialog dialog = new FileDialog(
                getShell(), SWT.OPEN);
        dialog.setText(dialogTitle);
        return dialog.open();
    }

    /**
     * If an error message is passed, the page is considered in error.
     * @param errorMessage null if page is OK, an error description otherwise
     */
    protected void updateStatus(final String errorMessage) {
        setErrorMessage(errorMessage);
        setPageComplete(errorMessage == null);
    }

    /**
     * @return the initial selection when this page is entered
     */
    public final IStructuredSelection getInitialSelection() {
        return mInitialSelection;
    }

    /**
     * @param initialSelection the initial selection when this page is entered
     *  to set
     */
    public final void setInitialSelection(
    		final IStructuredSelection initialSelection) {
        mInitialSelection = initialSelection;
    }

    /**
     * TODO move to AbstractLegStarWizardPage
     * Pops an error message.
     * @param shell parent shell
     * @param dialogTitle the error dialog title
     * @param pluginID the parent plugin ID
     * @param shortMessage text
     * @param reason a more detailed explanation
     */
    public static void errorDialog(
            final Shell shell,
            final String dialogTitle,
            final String pluginID,
            final String shortMessage,
            final String reason) {
    	AbstractWizard.errorDialog(
    			shell, dialogTitle, pluginID, shortMessage, reason);
    }
    
    /**
     * Log exception.
     * @param innerException exception
     * @param pluginID plugin ID
     */
    public static void logCoreException(
            final Throwable innerException,
            final String pluginID) {
    	AbstractWizard.logCoreException(innerException, pluginID);
    }
}
