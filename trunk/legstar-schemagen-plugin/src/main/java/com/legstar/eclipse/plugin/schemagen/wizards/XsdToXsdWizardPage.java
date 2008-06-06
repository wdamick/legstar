package com.legstar.eclipse.plugin.schemagen.wizards;

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;

import com.legstar.eclipse.plugin.schemagen.dialogs.HttpGetDialog;
import com.legstar.eclipse.plugin.schemagen.util.HttpClientHelper;
import com.legstar.eclipse.plugin.schemagen.util.HttpClientHelperException;

/**
 * This wizard page allows users to select an Xml Schema or a WSDL
 * source either from the file system or via an HTTP GET and 
 * generate a COBOL annotated Xml Schema.
 */
public class XsdToXsdWizardPage extends AbstractToXsdWizardPage {

    /** An XSD or WSDL source. */
    private Text mXsdSourceText;

    /** A class that provides Http get capabilities. */
    private HttpClientHelper mHttpHelper = new HttpClientHelper();

    /** Whether the input XSD/WSDL target namespace should be changed. */
    private boolean mSwitchNamespaceAllowed = false;

    /** No XSD or WSDL source error message. */
    private static final String NO_XSD_OR_WSDL_SOURCE_MSG =
        "You must provide XML Schema or WSDL source";
    
    /** A control that is enabled when user can switch the target namespace. */
    private Button mSwitchNamespaceCheckBox;
    
    /**
     * Constructs the wizard page.
     * @param initialSelection the workbench current selection
     */
    public XsdToXsdWizardPage(final IStructuredSelection initialSelection) {
        super(initialSelection,
                "XsdToXsdWizardPage",
                "Generate XML Schema from XML Schema or WSDL",
        "Select the XML Schema or WSDL source to be used for"
                + " COBOL-annotated XML Schema generation");
    }

    /** {@inheritDoc} */
    @Override
    protected void createExtendedControls(final Composite container) {
        createSelectXsdFromFileSystemLink(container);
        createSelectXsdFromWebSite(container);
        mSwitchNamespaceCheckBox =
        	createSwitchNamespaceAllowedCheckButton(container);
        mXsdSourceText = createMultilineTextField(container, LAYOUT_COLUMNS);
        FontData defaultFont = new FontData("Courier New", 8, SWT.NORMAL);
        Font font = new Font(container.getDisplay(), defaultFont);
        mXsdSourceText.setFont(font);
    }

    /**
     * This link will popup the resource selection dialog.
     * @param container the parent container
     */
    private void createSelectXsdFromFileSystemLink(final Composite container) {
        createHyperlink(container,
                "Select XML Schema or WSDL source from file system",
                PlatformUI.getWorkbench().getSharedImages().getImage(
                        ISharedImages.IMG_OBJ_FOLDER),
                        new HyperlinkAdapter() {
            public void linkActivated(final HyperlinkEvent e) {
                setXsdText(selectSingleFileContent(
                "Select an XML Schema or WSDL source"));
            }
        });
    }

    /**
     * This link will popup a custom dialog to issue an HTTP GET
     * on a remote Web Site.
     * @param container the parent container
     */
    private void createSelectXsdFromWebSite(
            final Composite container) {
        createHyperlink(container,
                "Select XML Schema or WSDL from Web site",
                PlatformUI.getWorkbench().getSharedImages().getImage(
                        ISharedImages.IMG_TOOL_FORWARD),
                        new HyperlinkAdapter() {
            public void linkActivated(final HyperlinkEvent e) {
                HttpGetDialog httpDialog =
                	new HttpGetDialog(container.getShell());
                if (httpDialog.open() == InputDialog.OK) {
                    try {
                        setXsdText(mHttpHelper.get(
                                httpDialog.getUrl(),
                                httpDialog.getUser(),
                                httpDialog.getPassword()));
                    } catch (HttpClientHelperException e1) {
                        MessageDialog.openError(
                                null,
                                HttpGetDialog.DIALOG_TITLE,
                                e1.getMessage());
                    }
                }
            }
        });
    }

    /**
     * Adds a check button that reflects its state in an associated boolean
     * variable.
     * @param container parent composite
     * @return a check button
     */
    private Button createSwitchNamespaceAllowedCheckButton(
            final Composite container) {
        final Button button = new Button(container, SWT.CHECK);
        button.setText("Switch target namespace");
        button.setSelection(isSwitchNamespaceAllowed());
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                setSwitchNamespaceAllowed(!isSwitchNamespaceAllowed());
                dialogChanged();
            }
        });
        final GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        gridData.horizontalSpan = LAYOUT_COLUMNS;
        gridData.horizontalIndent = 22;
        button.setLayoutData(gridData);
        return button;
    }
    
    /**
     * This will perform very minimal formatting assuming the content is XML.
     * @param xmlText the text to be reformatted
     */
    private void setXsdText(final String xmlText) {
        String formattedText = xmlText.replaceAll("><", ">\n<");
        mXsdSourceText.setText(formattedText);
    }

    /** {@inheritDoc} */
    @Override
    protected void dialogChanged() {
        if (mXsdSourceText.getText().length() > 0) {
            ((MainWizard) getWizard()).setCanFinish(true);
            updateStatus(null);
        } else {
            updateStatus(NO_XSD_OR_WSDL_SOURCE_MSG);
            ((MainWizard) getWizard()).setCanFinish(false);
        }
    }

    /** {@inheritDoc} */
   @Override
    protected void initContents() {
    }

    /**
     * {@inheritDoc}
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    public IWizardPage getNextPage() {
        return null;
    }

    /**
     * @return the Xml Schema or Wsdl source Text
     */
    public final Text getXsdSourceText() {
        return mXsdSourceText;
    }

    /**
     * @return true if the input XSD/WSDL target namespace should be changed
     */
    public final boolean isSwitchNamespaceAllowed() {
        return mSwitchNamespaceAllowed;
    }

    /**
     * @param switchNamespaceAllowed true if the input XSD/WSDL target namespace
     *  should be changed
     */
    public final void setSwitchNamespaceAllowed(
    		final boolean switchNamespaceAllowed) {
        mSwitchNamespaceAllowed = switchNamespaceAllowed;
    }

    /**
     * Depending on the presence of a new namespace we enable the capability to
     * switch to a different namespace.
     * @param newTargetNamespace the new target namespace to set
     */
    public final void setNewTargetNamespace(final String newTargetNamespace) {
        if (newTargetNamespace != null && newTargetNamespace.length() > 0) {
            mSwitchNamespaceCheckBox.setText(
            		"Switch namespace to " + newTargetNamespace);
            mSwitchNamespaceCheckBox.setEnabled(true);
        } else {
            mSwitchNamespaceCheckBox.setText("Switch namespace");
            mSwitchNamespaceCheckBox.setEnabled(false);
        }
    }

}
