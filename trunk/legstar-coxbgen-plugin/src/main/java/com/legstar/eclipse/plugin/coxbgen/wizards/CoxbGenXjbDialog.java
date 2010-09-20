package com.legstar.eclipse.plugin.coxbgen.wizards;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;
import com.legstar.eclipse.plugin.coxbgen.Messages;
import com.legstar.jaxb.gen.CobolJAXBXJBModel;

/**
 * A dialog box to collect JAXB customization parameters.
 */
public class CoxbGenXjbDialog extends TrayDialog {

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

    /** The XJB parameters. */
    private CobolJAXBXJBModel _xjbModel;

    /** A pattern to check for numericity. */
    private Pattern _numericPattern = Pattern.compile("[0-9]*");

    /**
     * Creates a dialog instance. Note that the dialog will have no visual
     * representation (no widgets) until it is told to open.
     * 
     * @param parentShell
     *            the parent shell
     * @param xjbModel the XJB parameters
     */
    protected CoxbGenXjbDialog(final Shell parentShell,
            final CobolJAXBXJBModel xjbModel) {
        super(parentShell);
        setTitle(Messages.jaxb_advanced_dialog_title);
        _xjbModel = xjbModel;
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
        final GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        gridLayout.verticalSpacing = 9;
        area.setLayout(gridLayout);

        _generateIsSetMethod = new Button(area, SWT.CHECK);
        _generateIsSetMethod.setText(Messages.xjb_generate_issetmethod_label);
        _generateIsSetMethod
                .setSelection(getXjbModel().isGenerateIsSetMethod());
        final GridData gridData = new GridData();
        gridData.horizontalSpan = 2;
        _generateIsSetMethod.setLayoutData(gridData);

        AbstractWizardPage.createLabel(area, Messages.xjb_serializableid_label);
        _serializableUid = AbstractWizardPage.createText(area);
        _serializableUid.setText(Long.toString(getXjbModel()
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

        AbstractWizardPage.createLabel(area, Messages.xjb_typenameprefix_label);
        _typeNamePrefix = AbstractWizardPage.createText(area);
        AbstractWizardPage.setTextFromString(_typeNamePrefix, getXjbModel()
                .getTypeNamePrefix());

        AbstractWizardPage.createLabel(area, Messages.xjb_typenamesuffix_label);
        _typeNameSuffix = AbstractWizardPage.createText(area);
        AbstractWizardPage.setTextFromString(_typeNameSuffix, getXjbModel()
                .getTypeNameSuffix());

        AbstractWizardPage.createLabel(area,
                Messages.xjb_elementnameprefix_label);
        _elementNamePrefix = AbstractWizardPage.createText(area);
        AbstractWizardPage.setTextFromString(_elementNamePrefix, getXjbModel()
                .getElementNamePrefix());

        AbstractWizardPage.createLabel(area,
                Messages.xjb_elementnamesuffix_label);
        _elementNameSuffix = AbstractWizardPage.createText(area);
        AbstractWizardPage.setTextFromString(_elementNameSuffix, getXjbModel()
                .getElementNameSuffix());

        return dialogArea;
    }

    /** {@inheritDoc} */
    public boolean close() {
        if (getReturnCode() == OK) {
            getXjbModel().setGenerateIsSetMethod(
                    _generateIsSetMethod.getSelection());
            getXjbModel().setSerializableUid(
                    Long.valueOf(_serializableUid.getText()));
            getXjbModel().setTypeNamePrefix(
                    AbstractWizardPage.getValueFromText(_typeNamePrefix));
            getXjbModel().setTypeNameSuffix(
                    AbstractWizardPage.getValueFromText(_typeNameSuffix));
            getXjbModel().setElementNamePrefix(
                    AbstractWizardPage.getValueFromText(_elementNamePrefix));
            getXjbModel().setElementNameSuffix(
                    AbstractWizardPage.getValueFromText(_elementNameSuffix));
        }
        return super.close();
    }

    /**
     * @return the XJB parameters
     */
    public CobolJAXBXJBModel getXjbModel() {
        return _xjbModel;
    }
}
