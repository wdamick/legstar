package com.legstar.eclipse.plugin.schemagen.dialogs;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.preferences.PreferenceConstants;

/**
 * This dialog prompts the user for a URL and basic authentication
 * parameters.
 */
public class HttpGetDialog extends Dialog {
    
    /** The dialog title. */
	public static final String DIALOG_TITLE = "HTTP GET dialog";
    
	/** Where to store dialog values that needs saving. */
	private IPreferenceStore mPreferenceStore;
    
    /** The URL queried for Xsd or Wsdl. */
	private String mUrl;
	
	/** The user ID to use for basic authentication. */
    private String mUser;
    
    /** The password used for basic authentication. */
    private String mPassword;

    /**
     * Construct the dialog for a given shell.
     * @param parentShell the parent shell
     */
    public HttpGetDialog(final Shell parentShell) {
        super(parentShell);
        try {
            mPreferenceStore = Activator.getDefault().getPreferenceStore();
        } catch (Exception e) {
            mPreferenceStore = null;
        }
    }

    /**
     * Contribute controls to the dialog.
     * {@inheritDoc}
     * @see org.eclipse.jface.dialogs.Dialog#
     * createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    public Control createDialogArea(final Composite parent) {
        parent.getShell().setText(DIALOG_TITLE);
        Composite container = (Composite) super.createDialogArea(parent);

        final GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        gridLayout.marginWidth = 5;
        container.setLayout(gridLayout);
        
        final Label urlLabel = new Label(container, SWT.NONE);
        urlLabel.setText("URL");
        final Text urlText = new Text(container, SWT.BORDER | SWT.SINGLE);
        urlText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        urlText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
               mUrl = urlText.getText();
               dialogChanged();
               saveUrlText(urlText);
            }
        });

        final Group basicAuth = new Group(container, SWT.SHADOW_ETCHED_IN);
        basicAuth.setText("Basic authentication");
        final GridData groupGridData = new GridData(GridData.FILL_HORIZONTAL);
        groupGridData.horizontalSpan = 2;
        basicAuth.setLayoutData(groupGridData);
        basicAuth.setLayout(new GridLayout(2, false));
        
        final Label userLabel = new Label(basicAuth, SWT.NONE);
        userLabel.setText("User");
        final Text userText = new Text(basicAuth, SWT.BORDER | SWT.SINGLE);
        userText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
              mUser = userText.getText();
              dialogChanged();
            }
        });
       
        final Label passwordLabel = new Label(basicAuth, SWT.NONE);
        passwordLabel.setText("Password");
        final Text passwordText = new Text(
        		basicAuth, SWT.BORDER | SWT.SINGLE | SWT.PASSWORD);
        passwordText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
               mPassword = passwordText.getText();
               dialogChanged();
            }
        });
       
        initUrlText(urlText);
        return container;
        
    }
    
    /**
     * Recover any previous values entered for the URL.
     * @param urlText the control to initialize
     */
    private void initUrlText(final Text urlText) {
        if (mPreferenceStore != null) {
            urlText.setText(mPreferenceStore.getString(
            		PreferenceConstants.LAST_URL_KEY));
        }
    }
    
    /**
     * Save the value entered for the URL.
     * @param urlText the control to save value from
     */
    private void saveUrlText(final Text urlText) {
        if (mPreferenceStore != null) {
            mPreferenceStore.putValue(
            		PreferenceConstants.LAST_URL_KEY, urlText.getText());
        }
    }

    /**
     * {@inheritDoc}
     * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
     */
    @Override
    protected Point getInitialSize() {
        Point proposedSize = super.getInitialSize();
        proposedSize.x += 300;
        return proposedSize;
    }
    
    /**
     * Perform validation whenever something changes on the dialog controls.
     */
    private void dialogChanged() {
        if (getButton(IDialogConstants.OK_ID) != null) {
            getButton(IDialogConstants.OK_ID).setEnabled(true);
            if (mUrl == null || mUrl.length() == 0) {
                getButton(IDialogConstants.OK_ID).setEnabled(false);
            } else {
                try {
                    URI url = new URI(mUrl);
                    url.toURL();
                } catch (URISyntaxException e) {
                    getButton(IDialogConstants.OK_ID).setEnabled(false);
                } catch (MalformedURLException e) {
                    getButton(IDialogConstants.OK_ID).setEnabled(false);
                } catch (IllegalArgumentException e) {
                    getButton(IDialogConstants.OK_ID).setEnabled(false);
                }
            }
        }
    }
    
    /**
     * @return the URL queried for Xsd or Wsdl
     */
    public String getUrl() {
        return mUrl;
    }
    
    /**
     * @return the user ID to use for basic authentication
     */
    public String getUser() {
        return mUser;
    }
    
    /**
     * @return the password used for basic authentication 
     */
    public String getPassword() {
        return mPassword;
    }
    

}
