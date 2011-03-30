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
package com.legstar.cixs.gen;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.options.PojoParameters;
import com.legstar.cixs.gen.model.options.WebServiceParameters;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.test.cixs.AbstractOperationCases;
import com.legstar.test.cixs.CultureinfoOperationCases;
import com.legstar.test.cixs.DplarchtOperationCases;
import com.legstar.test.cixs.JvmqueryOperationCases;
import com.legstar.test.cixs.JvmqueryWsOperationCases;
import com.legstar.test.cixs.LsfileacOperationCases;
import com.legstar.test.cixs.LsfileaeOperationCases;
import com.legstar.test.cixs.LsfilealOperationCases;
import com.legstar.test.cixs.MSNSearchOperationCases;

/**
 * Produces samples to use for generation testing.
 */
public final class Samples extends AbstractTestTemplate {

    /** Utility class. */
    private Samples() {

    }

    /**
     * Case with a regular commarea.
     * 
     * @return a service with a single operation mapping LSFILEAE
     */
    public static CixsJaxwsService getLsfileae() {
        CixsJaxwsService service = getNewService("lsfileae");
        service.addCixsOperation(LsfileaeOperationCases.getOperation(
                service.getName(), service.getPackageName()));
        return service;
    }

    /**
     * Case with different input and output commareas.
     * 
     * @return a service with a single operation mapping LSFILEAL
     */
    public static CixsJaxwsService getLsfileal() {
        CixsJaxwsService service = getNewService("lsfileal");
        service.addCixsOperation(LsfilealOperationCases.getOperation(
                service.getName(), service.getPackageName()));
        return service;
    }

    /**
     * Case with with multiple input and output containers.
     * 
     * @return a service with a single operation mapping LSFILEAC
     */
    public static CixsJaxwsService getLsfileac() {
        CixsJaxwsService service = getNewService("lsfileac");
        service.addCixsOperation(LsfileacOperationCases.getOperation(
                service.getName(), service.getPackageName()));
        return service;
    }

    /**
     * Case with with multiple operations.
     * 
     * @return a service with 2 operations, one mapping to LEFILEAE, the second
     *         one to LSFILEAC
     */
    public static CixsJaxwsService getLsfileax() {
        CixsJaxwsService service = getNewService("lsfileax");
        service.addCixsOperation(LsfileaeOperationCases.getOperation(
                "lsfileae", service.getPackageName()));
        service.addCixsOperation(LsfileacOperationCases.getOperation(
                "lsfileac", service.getPackageName()));
        return service;
    }

    /**
     * Case where the operation has a different package than the service.
     * 
     * @return a service with a single operation mapping LSFILEAE
     * */
    public static CixsJaxwsService getLsfilean() {
        CixsJaxwsService service = getNewService("lsfilean");
        CixsOperation operation = LsfileaeOperationCases.getOperation(
                "lsfileae", CIXS_PACKAGE_PREFIX + "lsfilean" + ".oper");
        service.addCixsOperation(operation);
        return service;
    }

    /**
     * Case where there is no operation package names (not even one inherited
     * from the service).
     * 
     * @return a service with a single operation mapping LSFILEAE
     * */
    public static CixsJaxwsService getLsfileap() {
        CixsJaxwsService service = getNewService("lsfileap");
        service.setPackageName(null);
        CixsOperation operation = LsfileaeOperationCases.getOperation(
                "lsfileae", service.getPackageName());
        service.addCixsOperation(operation);
        return service;
    }

    /**
     * Case with with single input and output containers.
     * 
     * @return a service with a single operation mapping LSFILEAC but using only
     *         one of the containers on input and on output
     * */
    public static CixsJaxwsService getLsfileac1() {
        CixsJaxwsService service = getNewService("lsfileac1");
        CixsOperation operation = LsfileacOperationCases.getOperation(
                "lsfileac", service.getPackageName());
        operation.getInput().remove(1);
        operation.getOutput().remove(1);
        service.addCixsOperation(operation);
        return service;
    }

    /**
     * Case with commarea driven but has multiple input and output structures.
     * 
     * @return a service with a single operation mapping LSFILEAM
     * */
    public static CixsJaxwsService getLsfileam() {
        CixsJaxwsService service = getNewService("lsfileam");
        CixsOperation operation = new CixsOperation();
        operation.setName("lsfileam");
        operation.setPackageName(service.getPackageName());
        operation.setCicsProgramName("LSFILEAM");
        operation.addInput(AbstractOperationCases.createCixsStructure(
                "lsfileac", "QueryData", "QueryData", false));
        operation.addInput(AbstractOperationCases.createCixsStructure(
                "lsfileac", "QueryLimit", "QueryLimit", false));
        operation.addOutput(AbstractOperationCases.createCixsStructure(
                "lsfileac", "ReplyStatus", "ReplyStatus", false));
        operation.addOutput(AbstractOperationCases.createCixsStructure(
                "lsfileac", "ReplyData", "ReplyData", false));
        service.addCixsOperation(operation);
        return service;
    }

    /**
     * Case with an adapter with custom code.
     * 
     * @return a service with custom code
     */
    public static CixsJaxwsService getDplarcht() {
        CixsJaxwsService service = getNewService("dplarcht");
        service.addCixsOperation(DplarchtOperationCases.getOperation(
                service.getName(), service.getPackageName()));
        return service;
    }

    /**
     * Case of a Web Service exposed to mainframe.
     * 
     * @return a service with a single operation mapping Cultureinfo getInfo
     *         operation
     * */
    public static CixsJaxwsService getCultureInfo() {
        CixsJaxwsService service = getNewService("cultureinfo");
        service.addCixsOperation(CultureinfoOperationCases.getOperation(
                service.getName(), service.getPackageName()));
        return service;
    }

    /**
     * Case of a POJO exposed to mainframe via a Web Service proxy.
     * 
     * @return a service with a single operation mapping Jvmquery queryJvm
     *         method
     * */
    public static CixsJaxwsService getJvmqueryWs() {
        CixsJaxwsService service = getNewService("jvmqueryWs");
        service.addCixsOperation(JvmqueryWsOperationCases.getOperation(
                service.getName(), service.getPackageName()));
        return service;
    }

    /**
     * Case of a POJO exposed to mainframe.
     * 
     * @return a service with a single operation mapping Jvmquery queryJvm
     *         method
     * */
    public static CixsJaxwsService getJvmquery() {
        CixsJaxwsService service = getNewService("jvmquery");
        service.addCixsOperation(JvmqueryOperationCases.getOperation(
                service.getName(), service.getPackageName()));
        return service;
    }

    /**
     * Case of a Web Service exposed to mainframe.
     * 
     * @return a service with a single operation mapping MSNSearch search
     *         operation
     * */
    public static CixsJaxwsService getMSNSearch() {
        CixsJaxwsService service = getNewService("MSNSearch");
        service.addCixsOperation(MSNSearchOperationCases.getOperation(
                service.getName(), service.getPackageName()));
        return service;
    }

    /**
     * @return target web service parameters for cultureinfo
     */
    public static WebServiceParameters getCultureinfoWebServiceParameters() {
        WebServiceParameters webServiceParameters = new WebServiceParameters();
        webServiceParameters
                .setWsdlUrl("http://localhost:8080/legstar-test-cultureinfo/getinfo?wsdl");
        webServiceParameters
                .setWsdlTargetNamespace("http://cultureinfo.cases.test.xsdc.legstar.com/");
        webServiceParameters.setWsdlServiceName("CultureInfoImplService");
        webServiceParameters.setWsdlPortName("CultureInfoImplPort");
        return webServiceParameters;

    }

    /**
     * @return target web service parameters for MSNSearch
     */
    public static WebServiceParameters getMSNSearchWebServiceParameters() {
        WebServiceParameters webServiceParameters = new WebServiceParameters();
        webServiceParameters
                .setWsdlUrl("http://soap.search.msn.com/webservices.asmx?wsdl");
        webServiceParameters
                .setWsdlTargetNamespace("http://schemas.microsoft.com/MSNSearch/2005/09/fex");
        webServiceParameters.setWsdlServiceName("MSNSearchService");
        webServiceParameters.setWsdlPortName("MSNSearchPort");
        return webServiceParameters;

    }

    /**
     * @return target pojo parameters for jvmquery
     */
    public static PojoParameters getJvmqueryPojoParameters() {
        PojoParameters pojoParameters = new PojoParameters();
        pojoParameters
                .setClassName("com.legstar.xsdc.test.cases.jvmquery.JVMQuery");
        pojoParameters.setMethodName("queryJvm");
        return pojoParameters;

    }

    /**
     * @return target web service parameters for Jvmquery
     */
    public static WebServiceParameters getJvmqueryWebServiceParameters() {
        WebServiceParameters webServiceParameters = new WebServiceParameters();
        webServiceParameters
                .setWsdlUrl("http://localhost:8080/legstar-test-jvmquery/queryJvm?wsdl");
        webServiceParameters
                .setWsdlTargetNamespace("http://jvmquery.cases.test.xsdc.legstar.com/");
        webServiceParameters.setWsdlServiceName("JVMQueryService");
        webServiceParameters.setWsdlPortName("JVMQueryPort");
        return webServiceParameters;

    }

}
