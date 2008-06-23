package com.legstar.eclipse.plugin.schemagen.wizards;

import java.io.StringWriter;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.common.wizards.IURLSelectionListener;
import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.util.XmlDocumentHelper;
import com.legstar.eclipse.plugin.schemagen.util.XmlDocumentHelperException;

/**
 * This wizard page allows users to select an Xml Schema or a WSDL
 * source either from the file system or over the network and 
 * generate a COBOL annotated Xml Schema.
 */
public class XsdToXsdWizardPage extends AbstractToXsdWizardPage {

    /** An XSD or WSDL source. */
    private Text mXsdSourceText;

    /** URL locating target XSD or WSDL. */
    private Combo mXsdUrlCombo = null;

    /** A class that provides XML loading/formatting capabilities. */
    private XmlDocumentHelper mXmlDocumentHelper = new XmlDocumentHelper();

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
    protected void createExtendedControls(final Composite container) {
        mXsdUrlCombo = createUrlComboGroup(
        		container, "XML Schema or WSDL",
        		new ModifyListener() {
                    public void modifyText(final ModifyEvent e) {
                        dialogChanged();
                    }
                },
        		new URLSelectionAdapter());
     	
        mSwitchNamespaceCheckBox =
        	createSwitchNamespaceAllowedCheckButton(container);
        mXsdSourceText = createMultilineTextField(container, LAYOUT_COLUMNS);
        FontData defaultFont = new FontData("Courier New", 8, SWT.NORMAL);
        Font font = new Font(container.getDisplay(), defaultFont);
        mXsdSourceText.setFont(font);
    }

	/**
	 *Defines what happens when a URL is selected.
	 */
	private class URLSelectionAdapter implements IURLSelectionListener {
	
		/** {@inheritDoc} */
		public void urlSelected(final String urlString) {
	    	try {
				mXmlDocumentHelper.load(getXsdUrl());
				StringWriter writer = new StringWriter();
				mXmlDocumentHelper.serialize(writer);
				mXsdSourceText.setText(writer.toString());
			} catch (XmlDocumentHelperException e1) {
				errorDialog(getShell(),
						"XML load error",
						Activator.PLUGIN_ID,
						"failed to load XML source \""
						+ urlString + "\"",
						e1.getMessage());
			}
		}
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
    
    /** {@inheritDoc} */
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
    protected void initContents() {
	   initUrlHistory();
    }

   /**
    * Setup the initial history list attached to the URL combo box.
    */
   private void initUrlHistory() {
	   for (String value : getUrlHistory().get()) {
		   mXsdUrlCombo.add(value);
	   }
   }

    /**
     * @return the Xml Schema or Wsdl source URI
     */
    public final String getXsdUrl() {
        return mXsdUrlCombo.getText();
    }
    
    /**
     * @return the Xml Schema or Wsdl source Text
     */
    public final Text getXsdSourceText() {
        return mXsdSourceText;
    }

    /**
     * {@inheritDoc}
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    public IWizardPage getNextPage() {
        return null;
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
