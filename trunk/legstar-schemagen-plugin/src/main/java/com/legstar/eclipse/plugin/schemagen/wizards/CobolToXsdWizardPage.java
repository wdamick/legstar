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
package com.legstar.eclipse.plugin.schemagen.wizards;

import java.util.Properties;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;

import com.legstar.eclipse.plugin.schemagen.Messages;
import com.legstar.eclipse.plugin.schemagen.preferences.PreferenceConstants;

/**
 * This wizard page allows users to select and edit the content of
 * a COBOL source. Alternatively, user can copy/paste COBOL code
 * directly and then generate an annotated XML Schema.
 */
public class CobolToXsdWizardPage extends AbstractToXsdWizardPage {

    /** A COBOL fragment. */
    private Text _cobolFragmentText;

    /** The File encoding to used for the COBOL code. */
    private String _cobolFileEncoding;

    /** A simple ruler to help with COBOL instructions entry. */
    private static final String COBOL_RULE_LABEL_LINE1 =
            "0--------1---------2---------3---------4---------5---------6"
                    + "---------7--    ";

    /**
     * Constructs the wizard page.
     * 
     * @param initialSelection the workbench current selection
     */
    public CobolToXsdWizardPage(final IStructuredSelection initialSelection) {
        super(initialSelection,
                "CobolToXsdWizardPage",
                Messages.cobol_To_xsd_wizard_page_title,
                Messages.cobol_To_xsd_wizard_page_description);
    }

    /** {@inheritDoc} */
    @Override
    public void createExtendedControls(final Composite container) {
        createSelectCobolFragmentsLink(container);
        Label lableLine1 = createLabel(
                container, COBOL_RULE_LABEL_LINE1, LAYOUT_COLUMNS);
        _cobolFragmentText = createMultilineTextField(
                container, LAYOUT_COLUMNS);
        FontData defaultFont = new FontData("Courier New", 8, SWT.NORMAL);
        Font font = new Font(container.getDisplay(), defaultFont);
        _cobolFragmentText.setFont(font);
        lableLine1.setFont(font);
        lableLine1.setBackground(
                container.getDisplay().getSystemColor(SWT.COLOR_GRAY));
    }

    /**
     * This link will popup the resource selection dialog.
     * 
     * @param container the parent container
     */
    private void createSelectCobolFragmentsLink(final Composite container) {
        createHyperlink(container,
                Messages.select_cobol_fragments_fs_label,
                PlatformUI.getWorkbench().getSharedImages().getImage(
                        ISharedImages.IMG_OBJ_FOLDER),
                new HyperlinkAdapter() {
                    public void linkActivated(final HyperlinkEvent e) {
                        _cobolFragmentText
                                .setText(
                                        selectSingleFileContent(
                                                Messages.select_cobol_fragments_dialog_title,
                                                getCobolFileEncoding()));
                    }
                });
    }

    /** {@inheritDoc} */
    @Override
    public void dialogChanged() {
        if (_cobolFragmentText.getText().length() > 0) {
            updateStatus(null);
        } else {
            updateStatus(Messages.no_cobol_fragment_selected_msg);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void initContents() {
        _cobolFileEncoding = getDefaultPreferences().getString(
                PreferenceConstants.COBOL_FILES_ENCODING);
        if (getProject() != null) {
            initProjectContent();
        }
    }

    /**
     * Project-related content can be initialized only when the project
     * has been identified. It is the responsibility of the previous wizard
     * page to identify a project in the Eclipse workspace and then call
     * this method.
     */
    public void initProjectContent() {
        _cobolFragmentText.setText(getProjectPreferences().get(
                PreferenceConstants.COBOL_CODE, ""));
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    public IWizardPage getNextPage() {
        return null;
    }

    /**
     * @return the Cobol Fragment Text
     */
    public String getCobolFragment() {
        return _cobolFragmentText.getText();
    }

    /**
     * @return the set of properties that need to be persisted
     */
    public Properties getPersistProperties() {
        Properties props = new Properties();
        props.put(PreferenceConstants.COBOL_CODE, _cobolFragmentText.getText());
        return props;
    }

    /**
     * @return the File encoding to used for the COBOL code
     */
    public String getCobolFileEncoding() {
        return _cobolFileEncoding;
    }

}
