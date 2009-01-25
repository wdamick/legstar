package com.legstar.eclipse.plugin.cixscom.wizards;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.legstar.cixs.jaxws.model.CobolHttpClientType;
import com.legstar.cixs.jaxws.model.HttpTransportParameters;
import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;

/**
 * The HTTP deployment control group.
 * <p/>
 * Parameters needed by Mainframe clients to reach a proxy over HTTP.
 *
 */
public abstract class AbstractCixsProxyDeployHttpGroup extends AbstractCixsControlsGroup {

    /** The Host address on which HTTP listens to mainframe clients. */
    private Text mHttpHostText = null;

    /** The Port on which HTTP listens to mainframe clients. */
    private Text mHttpPortText = null;

    /** The Path on which HTTP listens to mainframe clients. */
    private Text mHttpPathText = null;

    /** The user id for basic authentication. */
    private Text mHttpUserIdText = null;

    /** The password for basic authentication. */
    private Text mHttpPasswordText = null;

    /** Selection of DFHWBCLI sample Cobol http client type.*/
    private Button mDfhwbcliButton = null;

    /** Selection of WEB API sample Cobol http client type.*/
    private Button mWebapiButton = null;

    /** Selection of LegStar sample Cobol http client type.*/
    private Button mLegstarButton = null;

    /**
     * Construct this control group attaching it to a wizard page.
     * @param wizardPage the parent wizard page
     */
    public AbstractCixsProxyDeployHttpGroup(final AbstractCixsGeneratorWizardPage wizardPage) {
        super(wizardPage);
    }
    
    /**
     * {@inheritDoc} 
     */
    public void createButton(final Composite composite) {
        super.createButton(composite, "HTTP");
    }

    /**
     * {@inheritDoc} 
     */
    public void createControls(final Composite composite) {
        
        super.createControls(composite, Messages.http_transport_group_label, 2);

        AbstractWizardPage.createLabel(getGroup(), Messages.http_host_label + ':');
        mHttpHostText = AbstractWizardPage.createText(getGroup()); 

        AbstractWizardPage.createLabel(getGroup(), Messages.http_port_label + ':');
        mHttpPortText = AbstractWizardPage.createText(getGroup()); 

        AbstractWizardPage.createLabel(getGroup(), Messages.http_path_label + ':');
        mHttpPathText = AbstractWizardPage.createText(getGroup()); 

        AbstractWizardPage.createLabel(getGroup(), Messages.http_userid_label + ':');
        mHttpUserIdText = AbstractWizardPage.createText(getGroup()); 

        AbstractWizardPage.createLabel(getGroup(), Messages.http_password_label + ':');
        mHttpPasswordText = AbstractWizardPage.createText(getGroup()); 

        AbstractWizardPage.createLabel(getGroup(), Messages.sample_cobol_http_client_type_label + ':');
        Composite buttonsComposite = new Composite(getGroup(), SWT.NULL);
        buttonsComposite.setLayout(new RowLayout());

        mDfhwbcliButton = new Button(buttonsComposite, SWT.RADIO);
        mDfhwbcliButton.setText("CICS DFHWBCLI");

        mWebapiButton = new Button(buttonsComposite, SWT.RADIO);
        mWebapiButton.setText("CICS WEB API");

        mLegstarButton = new Button(buttonsComposite, SWT.RADIO);
        mLegstarButton.setText("LEGSTAR API");
    }

    /**
     * {@inheritDoc} 
     */
    public boolean validateControls() {
        if (getHttpHost() == null || getHttpHost().length() == 0) {
            getWizardPage().updateStatus(Messages.invalid_http_host_msg);
            return false;
        }
        try {
            if (Integer.parseInt(getHttpPort()) < 0 
                    || Integer.parseInt(getHttpPort()) > 65536) {
                getWizardPage().updateStatus(Messages.invalid_http_port_number_msg);
                return false;
            }
        } catch (NumberFormatException e) {
            getWizardPage().updateStatus(Messages.invalid_http_port_number_msg);
            return false;
        }
        if (getHttpPath() != null && getHttpPath().length() > 0) {
            if (getHttpPath().charAt(0) != '/') {
                getWizardPage().updateStatus(Messages.invalid_http_path_msg);
                return false;
            }
        }
        return true;
    }

    /**
     * {@inheritDoc} 
     */
    public void createExtendedListeners() {

        mHttpHostText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mHttpPortText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mHttpPathText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mHttpUserIdText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mHttpPasswordText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mDfhwbcliButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mWebapiButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mLegstarButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                getWizardPage().dialogChanged();
            }
        });
    }

    
    /**
     * @return the http parameters as a formatted http transport parameters object
     */
    public HttpTransportParameters getHttpTransportParameters() {
        HttpTransportParameters httpTransportParameters = new HttpTransportParameters();
        httpTransportParameters.setHost(getHttpHost());
        httpTransportParameters.setPort(Integer.parseInt(getHttpPort()));
        httpTransportParameters.setPath(getHttpPath());
        httpTransportParameters.setUserId(getHttpUserId());
        httpTransportParameters.setPassword(getHttpPassword());
        return httpTransportParameters;
    }
    
    /**
     * @return Host address on which HTTP listens to mainframe clients
     */
    public String getHttpHost() {
        return mHttpHostText.getText();
    }

    /**
     * @param httpHost Host address on which HTTP listens to mainframe clients
     */
    public void setHttpHost(final String httpHost) {
        mHttpHostText.setText(httpHost);
    }

    /**
     * @return Port on which HTTP listens to mainframe clients
     */
    public String getHttpPort() {
        return mHttpPortText.getText();
    }

    /**
     * @param httpPort Port on which HTTP listens to mainframe clients
     */
    public void setHttpPort(final String httpPort) {
        mHttpPortText.setText(httpPort);
    }

    /**
     * @return Path on which HTTP listens to mainframe clients
     */
    public String getHttpPath() {
        return mHttpPathText.getText();
    }

    /**
     * @param httpPath Path on which HTTP listens to mainframe clients
     */
    public void setHttpPath(final String httpPath) {
        mHttpPathText.setText(httpPath);
    }

    /**
     * @return UserId used for basic authentication
     */
    public String getHttpUserId() {
        return mHttpUserIdText.getText();
    }

    /**
     * @param httpUserId UserId used for basic authentication
     */
    public void setHttpUserId(final String httpUserId) {
        mHttpUserIdText.setText(httpUserId);
    }

    /**
     * @return Password used for basic authentication
     */
    public String getHttpPassword() {
        return mHttpPasswordText.getText();
    }

    /**
     * @param httpPassword Password used for basic authentication
     */
    public void setHttpPassword(final String httpPassword) {
        mHttpPasswordText.setText(httpPassword);
    }

    /**
     * @return the choice of sample Cobol client type selected
     */
    public final CobolHttpClientType getSampleCobolHttpClientType() {
        if (mDfhwbcliButton.getSelection()) {
            return CobolHttpClientType.DFHWBCLI;
        }
        if (mWebapiButton.getSelection()) {
            return CobolHttpClientType.WEBAPI;
        }
        if (mLegstarButton.getSelection()) {
            return CobolHttpClientType.LSHTTAPI;
        }
        return CobolHttpClientType.DFHWBCLI;
    }

    /**
     * @return the selection of DFHWBCLI sample Cobol http client type
     */
    public Button getDfhwbcliButton() {
        return mDfhwbcliButton;
    }

    /**
     * @param dfhwbcliButton the selection of DFHWBCLI sample Cobol http client type to set
     */
    public void setDfhwbcliButton(final Button dfhwbcliButton) {
        mDfhwbcliButton = dfhwbcliButton;
    }

    /**
     * @return the selection of WEB API sample Cobol http client type
     */
    public Button getWebapiButton() {
        return mWebapiButton;
    }

    /**
     * @param webapiButton the selection of WEB API sample Cobol http client type to set
     */
    public void setWebapiButton(final Button webapiButton) {
        mWebapiButton = webapiButton;
    }

    /**
     * @return the selection of LegStar sample Cobol http client type
     */
    public Button getLegstarButton() {
        return mLegstarButton;
    }

    /**
     * @param legstarButton the selection of LegStar sample Cobol http client type to set
     */
    public void setLegstarButton(final Button legstarButton) {
        mLegstarButton = legstarButton;
    }

}
