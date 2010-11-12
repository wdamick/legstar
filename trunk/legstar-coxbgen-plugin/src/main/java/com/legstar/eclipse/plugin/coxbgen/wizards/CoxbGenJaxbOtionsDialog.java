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
package com.legstar.eclipse.plugin.coxbgen.wizards;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;
import com.legstar.eclipse.plugin.coxbgen.Messages;
import com.legstar.jaxb.gen.JaxbGenModel;

/**
 * A dialog box to collect JAXB customization parameters.
 */
public class CoxbGenJaxbOtionsDialog extends TrayDialog {

    /** title of dialog. */
    private String _title;

    /** Whether to generate isset methods. */
    private Button _generateIsSetMethod;

    /** The serializable ID to use for generated classes. */
    private Text _serializableUid;

    /** Prefix to add to type names. */
    private Text _typeNamePrefix;

    /** Suffix to add to type names. */
    private Text _typeNameSuffix;

    /** Prefix to add to element names. */
    private Text _elementNamePrefix;

    /** Suffix to add to element names. */
    private Text _elementNameSuffix;

    /** The JAXB parameters. */
    private JaxbGenModel _jaxbgenModel;

    /** A pattern to check for numericity. */
    private Pattern _numericPattern = Pattern.compile("[0-9]*");

    /**
     * Creates a dialog instance. Note that the dialog will have no visual
     * representation (no widgets) until it is told to open.
     * 
     * @param parentShell
     *            the parent shell
     * @param jaxbgenModel the JAXB parameters
     */
    protected CoxbGenJaxbOtionsDialog(final Shell parentShell,
            final JaxbGenModel jaxbgenModel) {
        super(parentShell);
        setTitle(Messages.jaxb_options_dialog_title);
        setHelpAvailable(false);
        _jaxbgenModel = jaxbgenModel;
    }

    /**
     * Sets the title for this dialog.
     * 
     * @param title
     *            the title
     */
    public void setTitle(final String title) {
        _title = title;
    }

    /**
     * {@inheritDoc} (non-Javadoc) Method declared in Window.
     */
    protected void configureShell(final Shell shell) {
        super.configureShell(shell);
        if (_title != null) {
            shell.setText(_title);
        }
    }

    /**
     * {@inheritDoc} (non-Javadoc)
     * Method declared on Dialog.
     */
    protected Control createDialogArea(final Composite parent) {
        // create composite
        Composite area = (Composite) super.createDialogArea(parent);

        Group group = AbstractWizardPage.createGroup(area,
                Messages.jaxb_xjb_options_group_title, 2);

        _generateIsSetMethod = new Button(group, SWT.CHECK);
        _generateIsSetMethod
                .setText(Messages.jaxb_xjb_generate_issetmethod_label);
        _generateIsSetMethod
                .setSelection(getJaxbGenModel().isGenerateIsSetMethod());
        final GridData gridData = new GridData();
        gridData.horizontalSpan = 2;
        _generateIsSetMethod.setLayoutData(gridData);

        AbstractWizardPage
                .createLabel(group, Messages.jaxb_xjb_serializableid_label);
        _serializableUid = AbstractWizardPage.createText(group);
        _serializableUid.setText(Long.toString(getJaxbGenModel()
                .getSerializableUid()));
        _serializableUid.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(final VerifyEvent e) {
                Matcher m = _numericPattern.matcher(e.text);
                if (!m.matches()) {
                    e.doit = false;
                }
            }

        });

        AbstractWizardPage
                .createLabel(group, Messages.jaxb_xjb_typenameprefix_label);
        _typeNamePrefix = AbstractWizardPage.createText(group);
        AbstractWizardPage.setTextFromString(_typeNamePrefix, getJaxbGenModel()
                .getTypeNamePrefix());

        AbstractWizardPage
                .createLabel(group, Messages.jaxb_xjb_typenamesuffix_label);
        _typeNameSuffix = AbstractWizardPage.createText(group);
        AbstractWizardPage.setTextFromString(_typeNameSuffix, getJaxbGenModel()
                .getTypeNameSuffix());

        AbstractWizardPage.createLabel(group,
                Messages.jaxb_xjb_elementnameprefix_label);
        _elementNamePrefix = AbstractWizardPage.createText(group);
        AbstractWizardPage.setTextFromString(_elementNamePrefix,
                getJaxbGenModel()
                        .getElementNamePrefix());

        AbstractWizardPage.createLabel(group,
                Messages.jaxb_xjb_elementnamesuffix_label);
        _elementNameSuffix = AbstractWizardPage.createText(group);
        AbstractWizardPage.setTextFromString(_elementNameSuffix,
                getJaxbGenModel()
                        .getElementNameSuffix());

        return dialogArea;
    }

    /** {@inheritDoc} */
    public boolean close() {
        if (getReturnCode() == OK) {
            getJaxbGenModel().setGenerateIsSetMethod(
                    _generateIsSetMethod.getSelection());
            getJaxbGenModel().setSerializableUid(
                    Long.valueOf(_serializableUid.getText()));
            getJaxbGenModel().setTypeNamePrefix(
                    AbstractWizardPage.getValueFromText(_typeNamePrefix));
            getJaxbGenModel().setTypeNameSuffix(
                    AbstractWizardPage.getValueFromText(_typeNameSuffix));
            getJaxbGenModel().setElementNamePrefix(
                    AbstractWizardPage.getValueFromText(_elementNamePrefix));
            getJaxbGenModel().setElementNameSuffix(
                    AbstractWizardPage.getValueFromText(_elementNameSuffix));
        }
        return super.close();
    }

    /**
     * @return the XJB parameters
     */
    public JaxbGenModel getJaxbGenModel() {
        return _jaxbgenModel;
    }
}
