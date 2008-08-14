/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.jaxwsgen.dialogs;

import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.common.dialogs.AbstractDialog;
import com.legstar.eclipse.plugin.jaxwsgen.Messages;

/**
 * This dialog display elements from a WSDL and allows user the select a
 * service from the WSDL. Once a service is selected, user selects one of
 * services's ports.
 *
 */
public class WsdlPortSelectionDialog extends AbstractDialog {

	/** The WSDL URL to select port from. */
	private String mWsdlUrl;
	
	/** The list of available services extracted from the WSDL. */
	private List mServicesList;
	
	/** The list of available ports for a selected service. */
	private List mPortsList;
	
	/** A protected textbox showing the WSDL target namespace. */
	private Text mTargetNamespaceText;
	
	/** Selected target namespace. */
	private String mTargetNamespace;
	
	/** Selected service name. */
	private String mServiceName;
	
	/** Selected port name. */
	private String mPortName;
	
	/**
	 * Constructor for port selection dialog.
	 * @param wsdlUrl the WSDL URL to select port from
	 * @param pluginID the current plugin ID
	 * @param parentShell the parent shell
	 */
	public WsdlPortSelectionDialog(
			final String wsdlUrl,
			final Shell parentShell,
			final String pluginID) {
		super(parentShell, pluginID);
		mWsdlUrl = wsdlUrl;
	}

	/** {@inheritDoc}    */
	protected final Control createDialogArea(final Composite parent) {
		parent.getShell().setText(Messages.wsdl_port_selection_dialog_title);
		Composite composite = (Composite) super.createDialogArea(parent);
		try {
			initialize(composite);
		} catch (CoreException e) {
			errorDialog(Messages.wsdl_port_selection_error_dialog_title,
					NLS.bind(Messages.wsdl_access_error_msg,
							mWsdlUrl, e.getMessage()));
			close();
		}
		return composite;
	}

	/**
	 * Create dialog widgets.
	 * @param parent the parent composite
	 * @throws CoreException if creation fails
	 */
	private void initialize(final Composite parent) throws CoreException {

		Composite area = new Composite(parent, SWT.NULL);
		GridLayout gridLayout = new GridLayout(2, false);
		area.setLayout(gridLayout);
		
		createLabel(area, Messages.target_namespace_label + ':');
		mTargetNamespaceText = createText(area, "", -1);
		mTargetNamespaceText.setEditable(false);

		createLabel(area, Messages.services_list_label + ':');
		mServicesList = createList(area);
		mServicesList.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		mServicesList.addSelectionListener(
				new SelectionListener() {

					public void widgetDefaultSelected(
							final SelectionEvent arg0) {
						loadPorts();
					}

					public void widgetSelected(final SelectionEvent arg0) {
						loadPorts();
					}

				});
		
		createLabel(area, Messages.ports_list_label + ':');
		mPortsList = createList(area);
		mPortsList.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		mPortsList.addSelectionListener(
				new SelectionListener() {

					public void widgetDefaultSelected(
							final SelectionEvent arg0) {
						setSelection();
					}

					public void widgetSelected(final SelectionEvent arg0) {
						setSelection();
					}

				});
		
		loadWsdl(getWsdlUrl());
	}
	
	/**
	 * Use WSDL4J to load the subject WSDL and populate widgets.
	 * @param wsdlUrl the WSDL URL
	 * @throws CoreException of load fails
	 */
	@SuppressWarnings("unchecked")
	private void loadWsdl(final String wsdlUrl) throws CoreException {
        try {
			WSDLFactory factory = WSDLFactory.newInstance();
			WSDLReader reader = factory.newWSDLReader();
			Definition definition = reader.readWSDL(wsdlUrl);
			mTargetNamespaceText.setText(definition.getTargetNamespace());
	        Map < QName, Service > services = definition.getAllServices();
	        for (QName serviceKey : services.keySet()) {
	        	mServicesList.add(serviceKey.getLocalPart());
	        	mServicesList.setData(
	        			serviceKey.getLocalPart(), services.get(serviceKey));
	        }
	        /* Initially selects the first service available */
	        if (mServicesList.getItemCount() > 0) {
	        	mServicesList.select(0);
	        	loadPorts();
	        }
		} catch (WSDLException e) {
			throwCoreException(e);
		}
	}
	
	/**
	 * For the selected service retrieves all ports and populates the associated
	 * widget.
	 */
	@SuppressWarnings("unchecked")
	private void loadPorts() {
		Service service = (Service) mServicesList.getData(
				mServicesList.getSelection()[0]);
		Map < String, Port > ports = service.getPorts();
        mPortsList.removeAll();
        for (String portKey : ports.keySet()) {
        	mPortsList.add(portKey);
        }
        /* Initially selects the first port available */
        if (mPortsList.getItemCount() > 0) {
        	mPortsList.select(0);
        	setSelection();
        }
	}
	
	/**
	 * Make sure we store the last selection for later retrieval.
	 */
	private void setSelection() {
		mTargetNamespace = mTargetNamespaceText.getText();
		mServiceName = mServicesList.getSelection()[0];
		mPortName = mPortsList.getSelection()[0];
	}


	/**
	 * @return the Wsdl Url
	 */
	public final String getWsdlUrl() {
		return mWsdlUrl;
	}

	/**
	 * @return the Selected target namespace
	 */
	public final String getTargetNamespace() {
		return mTargetNamespace;
	}

	/**
	 * @param targetNamespace the Selected target namespace to set
	 */
	public final void setTargetNamespace(final String targetNamespace) {
		mTargetNamespace = targetNamespace;
	}

	/**
	 * @return the Selected service name
	 */
	public final String getServiceName() {
		return mServiceName;
	}

	/**
	 * @param serviceName the Selected service name to set
	 */
	public final void setServiceName(final String serviceName) {
		mServiceName = serviceName;
	}

	/**
	 * @return the Selected port name
	 */
	public final String getPortName() {
		return mPortName;
	}

	/**
	 * @param portName the Selected port name to set
	 */
	public final void setPortName(final String portName) {
		mPortName = portName;
	}

}
