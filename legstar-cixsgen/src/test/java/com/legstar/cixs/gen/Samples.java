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
package com.legstar.cixs.gen;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.test.cixs.CultureinfoOperationCases;
import com.legstar.test.cixs.JvmqueryWsOperationCases;
import com.legstar.test.cixs.LsfileacOperationCases;
import com.legstar.test.cixs.LsfileaeOperationCases;
import com.legstar.test.cixs.LsfilealOperationCases;
import com.legstar.test.cixs.MSNSearchOperationCases;

/**
 * Produces samples to use for generation testing.
 */
public final class Samples {

    /** Service classes package name prefix.*/
    private static final String CIXS_PACKAGE_PREFIX = "com.legstar.test.cixs.";

    /** Service namespace prefix.*/
    private static final String NAMESPACE_PREFIX = "http://cixs.test.legstar.com/";

    /** Utility class.*/
    private Samples() {
        
    }
    /**
     * Case with a regular commarea.
     * @return a service with a single operation mapping LSFILEAE
     */
    public static CixsJaxwsService getLsfileae() {
        CixsJaxwsService service = getNewService("lsfileae");
        service.getCixsOperations().add(
                LsfileaeOperationCases.getOperation(
                        service.getName(), service.getPackageName(), service.getTargetNamespace()));
        return service;
    }

    /**
     * Case with different input and output commareas.
     * @return a service with a single operation mapping LSFILEAL
     */
    public static CixsJaxwsService getLsfileal() {
        CixsJaxwsService service = getNewService("lsfileal");
        service.getCixsOperations().add(
                LsfilealOperationCases.getOperation(
                        service.getName(), service.getPackageName(), service.getTargetNamespace()));
        return service;
    }

    /**
     * Case with with multiple input and output containers.
     * @return a service with a single operation mapping LSFILEAC
     */
    public static CixsJaxwsService getLsfileac() {
        CixsJaxwsService service = getNewService("lsfileac");
        service.getCixsOperations().add(
                LsfileacOperationCases.getOperation(
                        service.getName(), service.getPackageName(), service.getTargetNamespace()));
        return service;
    }

    /**
     * Case with with multiple operations.
     * @return a service with 2 operations, one mapping to LEFILEAE, the second
     * one to LSFILEAC
     */
    public static CixsJaxwsService getLsfileax() {
        CixsJaxwsService service = getNewService("lsfileax");
        service.getCixsOperations().add(
                LsfileaeOperationCases.getOperation(
                        service.getName(), service.getPackageName(), service.getTargetNamespace()));
        service.getCixsOperations().add(
                LsfileacOperationCases.getOperation(
                        service.getName(), service.getPackageName(), service.getTargetNamespace()));
        return service;
    }

    /**
     * Case where the operation has a different namespace/package than the service.
     * @return a service with a single operation mapping LSFILEAE
     *  */
    public static CixsJaxwsService getLsfilean() {
        CixsJaxwsService service = getNewService("lsfilean");
        CixsOperation operation = 
            LsfileaeOperationCases.getOperation(
                    service.getName(),
                    CIXS_PACKAGE_PREFIX + "oper." + "lsfilean",
                    NAMESPACE_PREFIX + "oper/" + "lsfilean");
        service.getCixsOperations().add(operation);
        return service;
    }

    /**
     * Case where there is no operation package names (not even one inherited
     * from the service).
     * @return a service with a single operation mapping LSFILEAE
     *  */
    public static CixsJaxwsService getLsfileap() {
        CixsJaxwsService service = getNewService("lsfileap");
        CixsOperation operation = LsfileaeOperationCases.getOperation(
                    service.getName(), null, service.getTargetNamespace());
        service.setPackageName(null);
        service.getCixsOperations().add(operation);
        return service;
    }

    /**
     * Case with with single input and output containers.
     * @return a service with a single operation mapping LSFILEAC but using only
     * one of the containers on input and on output
     *  */
    public static CixsJaxwsService getLsfileaq() {
        CixsJaxwsService service = getNewService("lsfileaq");
        CixsOperation operation = LsfileacOperationCases.getOperation(
                service.getName(), service.getPackageName(), service.getTargetNamespace());
        operation.getInput().remove(1);
        operation.getOutput().remove(1);
        service.getCixsOperations().add(operation);
        return service;
    }

    /**
     * Case of a POJO exposed to mainframe via a Web Service proxy.
     * @return a service with a single operation mapping Jvmquery queryJvm method
     *  */
    public static CixsJaxwsService getJvmqueryWs() {
        CixsJaxwsService service = getNewService("jvmqueryWs");
        service.getCixsOperations().add(
                JvmqueryWsOperationCases.getOperation(
                        service.getName(), service.getPackageName(), service.getTargetNamespace()));
        service.setTargetNamespace("http://jvmquery.cases.test.xsdc.legstar.com/");
        service.setWsdlUrl("http://localhost:8080/jaxws-jvmquery/queryJvm?wsdl");
        service.setWsdlServiceName("JvmqueryImplService");
        service.setWsdlPortName("JvmqueryImplPort");
        service.setServiceURI(service.getDefaultServiceURI());
        service.setServiceUserId("alice");
        service.setServicePassword("inwonderland");
        return service;
    }

    /**
     * Case of a Web Service exposed to mainframe.
     * @return a service with a single operation mapping Cultureinfo getInfo operation
     *  */
    public static CixsJaxwsService getCultureInfo() {
        CixsJaxwsService service = getNewService("cultureinfo");
        service.getCixsOperations().add(
                CultureinfoOperationCases.getOperation(
                        service.getName(), service.getPackageName(), service.getTargetNamespace()));
        service.setTargetNamespace("http://cultureinfo.cases.test.xsdc.legstar.com/");
        service.setWsdlUrl("http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl");
        service.setWsdlServiceName("CultureInfoImplService");
        service.setWsdlPortName("CultureInfoImplPort");
        service.setServiceURI(service.getDefaultServiceURI());
        return service;
    }

    /**
     * Case of a Web Service exposed to mainframe.
     * @return a service with a single operation mapping MSNSearch search operation
     *  */
    public static CixsJaxwsService getMSNSearch() {
        CixsJaxwsService service = getNewService("MSNSearch");
        service.getCixsOperations().add(
                MSNSearchOperationCases.getOperation(
                        service.getName(), service.getPackageName(), service.getTargetNamespace()));
        service.setTargetNamespace("http://schemas.microsoft.com/MSNSearch/2005/09/fex");
        service.setWsdlUrl("http://soap.search.msn.com/webservices.asmx?wsdl");
        service.setWsdlServiceName("MSNSearchService");
        service.setWsdlPortName("MSNSearchPort");
        service.setServiceURI(service.getDefaultServiceURI());
        return service;
    }

    /**
     * Create a service without any operations.
     * @param serviceName the service name
     * @return a new service
     */
    public static CixsJaxwsService getNewService(final String serviceName) {
        CixsJaxwsService model = new CixsJaxwsService();
        model.setPackageName(CIXS_PACKAGE_PREFIX + serviceName);
        model.setImplementationClassName(
                CodeGenUtil.classNormalize(serviceName) + "Impl");
        model.setInterfaceClassName(
                CodeGenUtil.classNormalize(serviceName));
        model.setName(serviceName);
        model.setTargetNamespace(NAMESPACE_PREFIX + serviceName);
        return model;
    }

}
