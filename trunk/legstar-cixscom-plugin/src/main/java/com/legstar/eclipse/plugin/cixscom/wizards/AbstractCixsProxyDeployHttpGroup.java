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

import com.legstar.cixs.gen.model.options.CobolHttpClientType;
import com.legstar.cixs.gen.model.options.HttpTransportParameters;
import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;

/**
 * The HTTP deployment control group.
 * <p/>
 * Parameters needed by Mainframe clients to reach a proxy over HTTP.
 * 
 */
public abstract class AbstractCixsProxyDeployHttpGroup extends
        AbstractCixsControlsGroup {

    /** The Host address on which HTTP listens to mainframe clients. */
    private Text _httpHostText = null;

    /** The Port on which HTTP listens to mainframe clients. */
    private Text _httpPortText = null;

    /** The Path on which HTTP listens to mainframe clients. */
    private Text _httpPathText = null;

    /** The user id for basic authentication. */
    private Text _httpUserIdText = null;

    /** The password for basic authentication. */
    private Text _httpPasswordText = null;

    /** Selection of DFHWBCLI sample Cobol http client type. */
    private Button _dfhwbcliButton = null;

    /** Selection of WEB API sample Cobol http client type. */
    private Button _webapiButton = null;

    /** Selection of LegStar sample Cobol http client type. */
    private Button _legstarButton = null;

    /** The data model. */
    private HttpTransportParameters _genModel;

    /** The initial HTTP sample client type. */
    private CobolHttpClientType _sampleCobolHttpClientType;

    /**
     * Construct this control group attaching it to a wizard page.
     * 
     * @param wizardPage the parent wizard page
     * @param genModel the data model
     * @param sampleCobolHttpClientType initial HTTP sample client type
     * @param selected whether this group should initially be selected
     */
    public AbstractCixsProxyDeployHttpGroup(
            final AbstractCixsGeneratorWizardPage wizardPage,
            final HttpTransportParameters genModel,
            final CobolHttpClientType sampleCobolHttpClientType,
            final boolean selected) {
        super(wizardPage, selected);
        _genModel = genModel;
        _sampleCobolHttpClientType = sampleCobolHttpClientType;
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

        AbstractWizardPage.createLabel(getGroup(),
                Messages.http_host_label + ':');
        _httpHostText = AbstractWizardPage.createText(getGroup());

        AbstractWizardPage.createLabel(getGroup(),
                Messages.http_port_label + ':');
        _httpPortText = AbstractWizardPage.createText(getGroup());

        AbstractWizardPage.createLabel(getGroup(),
                Messages.http_path_label + ':');
        _httpPathText = AbstractWizardPage.createText(getGroup());

        AbstractWizardPage.createLabel(getGroup(),
                Messages.http_userid_label + ':');
        _httpUserIdText = AbstractWizardPage.createText(getGroup());

        AbstractWizardPage.createLabel(getGroup(),
                Messages.http_password_label + ':');
        _httpPasswordText = AbstractWizardPage.createText(getGroup());

        AbstractWizardPage.createLabel(getGroup(),
                Messages.sample_cobol_http_client_type_label + ':');
        Composite buttonsComposite = new Composite(getGroup(), SWT.NULL);
        buttonsComposite.setLayout(new RowLayout());

        _dfhwbcliButton = new Button(buttonsComposite, SWT.RADIO);
        _dfhwbcliButton.setText("CICS DFHWBCLI");

        _webapiButton = new Button(buttonsComposite, SWT.RADIO);
        _webapiButton.setText("CICS WEB API");

        _legstarButton = new Button(buttonsComposite, SWT.RADIO);
        _legstarButton.setText("LEGSTAR API");
    }

    /**
     * {@inheritDoc}
     */
    public void initExtendedControls() {
        setHttpHost(getInitHttpHost());
        setHttpPort(getInitHttpPort());
        setHttpPath(getInitHttpPath());
        setHttpUserId(getInitHttpUserId());
        setHttpPassword(getInitHttpPassword());
        getDfhwbcliButton().setSelection(
                _sampleCobolHttpClientType == CobolHttpClientType.DFHWBCLI);
        getWebapiButton().setSelection(
                _sampleCobolHttpClientType == CobolHttpClientType.WEBAPI);
        getLegstarButton().setSelection(
                _sampleCobolHttpClientType == CobolHttpClientType.LSHTTAPI);
    }

    /**
     * @return a safe initial value
     */
    protected String getInitHttpHost() {
        String initValue = _genModel.getHost();
        if (initValue == null) {
            initValue = getDefaultHttpHost();
        }
        return initValue;
    }

    /**
     * @return a safe initial value
     */
    protected String getInitHttpPort() {
        int initValue = _genModel.getPort();
        if (initValue == HttpTransportParameters.PORT_NOT_SET) {
            initValue = getDefaultHttpPort();
        }
        return Integer.toString(initValue);
    }

    /**
     * @return a safe initial value
     */
    protected String getInitHttpPath() {
        String initValue = _genModel.getPath();
        if (initValue == null) {
            initValue = getDefaultHttpPath();
        }
        return initValue;
    }

    /**
     * @return a safe initial value
     */
    protected String getInitHttpUserId() {
        String initValue = _genModel.getUserId();
        if (initValue == null) {
            initValue = getDefaultHttpUserId();
        }
        return initValue;
    }

    /**
     * @return a safe initial value
     */
    protected String getInitHttpPassword() {
        String initValue = _genModel.getPassword();
        if (initValue == null) {
            initValue = getDefaultHttpPassword();
        }
        return initValue;
    }

    /**
     * @return a default HTTP host
     */
    public abstract String getDefaultHttpHost();

    /**
     * @return a default HTTP port number
     */
    public abstract int getDefaultHttpPort();

    /**
     * @return a default HTTP path
     */
    public abstract String getDefaultHttpPath();

    /**
     * @return a default HTTP basic authentication user ID
     */
    public abstract String getDefaultHttpUserId();

    /**
     * @return a default HTTP basic authentication password
     */
    public abstract String getDefaultHttpPassword();

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
                getWizardPage().updateStatus(
                        Messages.invalid_http_port_number_msg);
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

        _httpHostText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _httpPortText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _httpPathText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _httpUserIdText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _httpPasswordText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _dfhwbcliButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _webapiButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _legstarButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                getWizardPage().dialogChanged();
            }
        });
    }

    /** {@inheritDoc} */
    @Override
    public void updateGenModelExtended() {
        getGenModel().setHost(getHttpHost());
        getGenModel().setPort(Integer.parseInt(getHttpPort()));
        getGenModel().setPath(getHttpPath());
        getGenModel().setUserId(getHttpUserId());
        getGenModel().setPassword(getHttpPassword());
        if (_dfhwbcliButton.getSelection()) {
            _sampleCobolHttpClientType = CobolHttpClientType.DFHWBCLI;
        }
        if (_webapiButton.getSelection()) {
            _sampleCobolHttpClientType = CobolHttpClientType.WEBAPI;
        }
        if (_legstarButton.getSelection()) {
            _sampleCobolHttpClientType = CobolHttpClientType.LSHTTAPI;
        }
    }

    /**
     * @return Host address on which HTTP listens to mainframe clients
     */
    public String getHttpHost() {
        return _httpHostText.getText();
    }

    /**
     * @param httpHost Host address on which HTTP listens to mainframe clients
     */
    public void setHttpHost(final String httpHost) {
        _httpHostText.setText(httpHost);
    }

    /**
     * @return Port on which HTTP listens to mainframe clients
     */
    public String getHttpPort() {
        return _httpPortText.getText();
    }

    /**
     * @param httpPort Port on which HTTP listens to mainframe clients
     */
    public void setHttpPort(final String httpPort) {
        _httpPortText.setText(httpPort);
    }

    /**
     * @return Path on which HTTP listens to mainframe clients
     */
    public String getHttpPath() {
        return _httpPathText.getText();
    }

    /**
     * @param httpPath Path on which HTTP listens to mainframe clients
     */
    public void setHttpPath(final String httpPath) {
        _httpPathText.setText(httpPath);
    }

    /**
     * @return UserId used for basic authentication
     */
    public String getHttpUserId() {
        return _httpUserIdText.getText();
    }

    /**
     * @param httpUserId UserId used for basic authentication
     */
    public void setHttpUserId(final String httpUserId) {
        _httpUserIdText.setText(httpUserId);
    }

    /**
     * @return Password used for basic authentication
     */
    public String getHttpPassword() {
        return _httpPasswordText.getText();
    }

    /**
     * @param httpPassword Password used for basic authentication
     */
    public void setHttpPassword(final String httpPassword) {
        _httpPasswordText.setText(httpPassword);
    }

    /**
     * @return the choice of sample Cobol client type selected
     */
    public CobolHttpClientType getSampleCobolHttpClientType() {
        return _sampleCobolHttpClientType;
    }

    /**
     * @return the selection of DFHWBCLI sample Cobol http client type
     */
    public Button getDfhwbcliButton() {
        return _dfhwbcliButton;
    }

    /**
     * @param dfhwbcliButton the selection of DFHWBCLI sample Cobol http client
     *            type to set
     */
    public void setDfhwbcliButton(final Button dfhwbcliButton) {
        _dfhwbcliButton = dfhwbcliButton;
    }

    /**
     * @return the selection of WEB API sample Cobol http client type
     */
    public Button getWebapiButton() {
        return _webapiButton;
    }

    /**
     * @param webapiButton the selection of WEB API sample Cobol http client
     *            type to set
     */
    public void setWebapiButton(final Button webapiButton) {
        _webapiButton = webapiButton;
    }

    /**
     * @return the selection of LegStar sample Cobol http client type
     */
    public Button getLegstarButton() {
        return _legstarButton;
    }

    /**
     * @param legstarButton the selection of LegStar sample Cobol http client
     *            type to set
     */
    public void setLegstarButton(final Button legstarButton) {
        _legstarButton = legstarButton;
    }

    /**
     * @return the data model associated with this group
     */
    public HttpTransportParameters getGenModel() {
        return _genModel;
    }

}
