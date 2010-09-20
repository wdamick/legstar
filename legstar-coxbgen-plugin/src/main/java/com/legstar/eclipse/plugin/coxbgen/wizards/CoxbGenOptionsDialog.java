package com.legstar.eclipse.plugin.coxbgen.wizards;

import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.legstar.coxb.gen.CoxbGenModel;
import com.legstar.eclipse.plugin.coxbgen.Messages;

/**
 * A dialog box to collect COXB generation parameters.
 */
public class CoxbGenOptionsDialog extends TrayDialog {

    /** title of dialog. */
    private String _title;

    /** An instance of the generator model. */
    CoxbGenModel _coxbModel;

    /** Whether to generate XML Transformers. */
    private Button _generateXmlTransformers;

    /**
     * Creates a dialog instance. Note that the dialog will have no visual
     * representation (no widgets) until it is told to open.
     * 
     * @param parentShell
     *            the parent shell
     * @param coxbModel the COXB generator model
     */
    protected CoxbGenOptionsDialog(final Shell parentShell,
            final CoxbGenModel coxbModel) {
        super(parentShell);
        setTitle(Messages.coxb_options_dialog_title);
        _coxbModel = coxbModel;
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

        _generateXmlTransformers = new Button(area, SWT.CHECK);
        _generateXmlTransformers
                .setText(Messages.coxb_options_xmltransformers_label);
        _generateXmlTransformers
                .setSelection(getCoxbModel().isXmlTransformers());
        final GridData gridData = new GridData();
        gridData.horizontalSpan = 2;
        _generateXmlTransformers.setLayoutData(gridData);

        return dialogArea;
    }

    /** {@inheritDoc} */
    public boolean close() {
        if (getReturnCode() == OK) {
            getCoxbModel().setXmlTransformers(
                    _generateXmlTransformers.getSelection());
        }
        return super.close();
    }

    /**
     * @return the COXB generator model
     */
    public CoxbGenModel getCoxbModel() {
        return _coxbModel;
    }
}
