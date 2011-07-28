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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

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
public abstract class AbstractCixsProxyHttpGroup extends
        AbstractCixsHttpGroup {

    /** Selection of DFHWBCLI sample Cobol http client type. */
    private Button _dfhwbcliButton = null;

    /** Selection of WEB API sample Cobol http client type. */
    private Button _webapiButton = null;

    /** Selection of LegStar sample Cobol http client type. */
    private Button _legstarButton = null;

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
    public AbstractCixsProxyHttpGroup(
            final AbstractCixsGeneratorWizardPage wizardPage,
            final HttpTransportParameters genModel,
            final CobolHttpClientType sampleCobolHttpClientType,
            final boolean selected) {
        super(wizardPage, genModel, selected,
                Messages.proxy_http_transport_group_label);
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

        super.createControls(composite);

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
        super.initExtendedControls();
        getDfhwbcliButton().setSelection(
                _sampleCobolHttpClientType == CobolHttpClientType.DFHWBCLI);
        getWebapiButton().setSelection(
                _sampleCobolHttpClientType == CobolHttpClientType.WEBAPI);
        getLegstarButton().setSelection(
                _sampleCobolHttpClientType == CobolHttpClientType.LSHTTAPI);
    }

    /**
     * {@inheritDoc}
     */
    public boolean validateControls() {
        if (!super.validateControls()) {
            return false;
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public void createExtendedListeners() {
        super.createExtendedListeners();

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
        super.updateGenModelExtended();

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

}
