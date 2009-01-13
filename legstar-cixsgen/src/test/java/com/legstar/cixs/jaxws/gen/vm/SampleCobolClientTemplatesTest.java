package com.legstar.cixs.jaxws.gen.vm;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator;
import com.legstar.cixs.jaxws.gen.StructuresGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;


/**
 * Test the sample COBOL WMQ Client generation.
 *
 */
public class SampleCobolClientTemplatesTest extends AbstractTestTemplate {

    /**
     * Generate the c2ws sample COBOL client and test it.
     * @throws Exception if generation fails
     */
    public void testCobolCicsC2wsJvmquery() throws Exception {

        CixsJaxwsService model = Samples.getJvmqueryWs();
        getParameters().put("cixsOperation", model.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        String resStr = genSource(model,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_LSHTTAPI_CLIENT_VLC_TEMPLATE,
                GEN_COBOL_DIR,
                model.getCixsOperations().get(0).getCicsProgramName() + ".cbl");
        String url = "http://" + CodeGenUtil.getLocalIPAddress() + ":8080/c2ws-jvmqueryWs/jvmqueryWsProxy";

        assertTrue(resStr.contains("       PROGRAM-ID. JVMQUERY."));
        assertTrue(resStr.contains("77  W00-SERVICE-URI               PIC X(" + url.length() + ") VALUE"));
        assertTrue(resStr.contains("'" + url + "'."));
        assertTrue(resStr.contains("77  W00-USERID                    PIC X(5) VALUE"));
        assertTrue(resStr.contains("'alice'."));
        assertTrue(resStr.contains("77  W00-PASSWORD                  PIC X(12) VALUE"));
        assertTrue(resStr.contains("'inwonderland'."));
        assertTrue(resStr.contains("77  W00-SERVICE-NAME              PIC X(10) VALUE"));
        assertTrue(resStr.contains("'jvmqueryWs'."));
        assertTrue(resStr.contains("           02 QueryJvm."));
        assertTrue(resStr.contains("               03 envVarNames--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("               03 arg0."));
        assertTrue(resStr.contains("                   04 envVarNames PIC X(32) OCCURS 0 TO 10 DEPENDING ON"));
        assertTrue(resStr.contains("                       envVarNames--C."));
        assertTrue(resStr.contains("           02 QueryJvmResponse."));
        assertTrue(resStr.contains("               03 envVarValues--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("               03 R-return."));
        assertTrue(resStr.contains("                   04 country PIC X(32)."));
        assertTrue(resStr.contains("                   04 currencySymbol PIC X(32)."));
        assertTrue(resStr.contains("                   04 envVarValues PIC X(32) OCCURS 0 TO 10 DEPENDING ON"));
        assertTrue(resStr.contains("                       envVarValues--C."));
        assertTrue(resStr.contains("                   04 formattedDate PIC X(32)."));
        assertTrue(resStr.contains("                   04 language PIC X(32)."));
        assertTrue(resStr.contains("77  W00-SERVICE-NAME              PIC X(10) VALUE"));
        assertTrue(resStr.contains("'JVMQUERY STARTING ==============================='"));
        assertTrue(resStr.contains("MOVE 'JVMQUERY' TO LAPI-TRACE-ID."));
    }

    /**
     * Generate the c2ws sample COBOL client and test it.
     * @throws Exception if generation fails
     */
    public void testCobolCicsC2wsCultureinfo() throws Exception {

        CixsJaxwsService model = Samples.getCultureInfo();
        getParameters().put("cixsOperation", model.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        String resStr = genSource(model,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_LSHTTAPI_CLIENT_VLC_TEMPLATE,
                GEN_COBOL_DIR,
                model.getCixsOperations().get(0).getCicsProgramName() + ".cbl");

        String url = "http://" + CodeGenUtil.getLocalIPAddress() + ":8080/c2ws-cultureinfo/cultureinfoProxy";

        assertTrue(resStr.contains("       PROGRAM-ID. CULTUREI."));
        assertTrue(resStr.contains("77  W00-SERVICE-URI               PIC X(" + url.length() + ") VALUE"));
        assertTrue(resStr.contains("'" + url + "'."));
        assertTrue(resStr.contains("77  W00-USERID                    PIC X(8) VALUE"));
        assertTrue(resStr.contains("'        '."));
        assertTrue(resStr.contains("77  W00-PASSWORD                  PIC X(8) VALUE"));
        assertTrue(resStr.contains("'        '."));
        assertTrue(resStr.contains("77  W00-SERVICE-NAME              PIC X(11) VALUE"));
        assertTrue(resStr.contains("'cultureinfo'."));
        assertTrue(resStr.contains("           02 GetInfo."));
        assertTrue(resStr.contains("               03 arg0."));
        assertTrue(resStr.contains("                   04 cultureCode PIC X(32)."));
        assertTrue(resStr.contains("                   04 decimalNumber PIC 9(7)V9(2) COMP-3."));
        assertTrue(resStr.contains("           02 GetInfoResponse."));
        assertTrue(resStr.contains("               03 R-return."));
        assertTrue(resStr.contains("                   04 currencySymbol PIC X(32)."));
        assertTrue(resStr.contains("                   04 displayCountry PIC X(32)."));
        assertTrue(resStr.contains("                   04 displayLanguage PIC X(32)."));
        assertTrue(resStr.contains("                   04 formattedDate PIC X(32)."));
        assertTrue(resStr.contains("                   04 formattedDecimalNumber PIC X(32)."));
        assertTrue(resStr.contains("                   04 serverCultureInfo."));
        assertTrue(resStr.contains("                       05 cultureCode PIC X(32)."));
        assertTrue(resStr.contains("                       05 displayCountry0 PIC X(32)."));
        assertTrue(resStr.contains("                       05 displayLanguage0 PIC X(32)."));
        assertTrue(resStr.contains("'CULTUREI STARTING ==============================='"));
        assertTrue(resStr.contains("'CULTUREI STOPPING ==============================='"));
    }

    /**
     * Generate the c2ws sample COBOL client and test it.
     * @throws Exception if generation fails
     */
    public void testCobolCicsC2wsMSNSearch() throws Exception {

        CixsJaxwsService model = Samples.getMSNSearch();
        getParameters().put("cixsOperation", model.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        String resStr = genSource(model,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_LSHTTAPI_CLIENT_VLC_TEMPLATE,
                GEN_COBOL_DIR,
                model.getCixsOperations().get(0).getCicsProgramName() + ".cbl");

        String url = "http://" + CodeGenUtil.getLocalIPAddress() + ":8080/c2ws-MSNSearch/MSNSearchProxy";

        assertTrue(resStr.contains("       PROGRAM-ID. MSNSEARC."));
        assertTrue(resStr.contains("77  W00-SERVICE-URI               PIC X(" + url.length() + ") VALUE"));
        assertTrue(resStr.contains("'" + url + "'."));
        assertTrue(resStr.contains("77  W00-USERID                    PIC X(8) VALUE"));
        assertTrue(resStr.contains("'        '."));
        assertTrue(resStr.contains("77  W00-PASSWORD                  PIC X(8) VALUE"));
        assertTrue(resStr.contains("'        '."));
        assertTrue(resStr.contains("77  W00-SERVICE-NAME              PIC X(9) VALUE"));
        assertTrue(resStr.contains("'MSNSearch'."));
        assertTrue(resStr.contains("           02 R-Search."));
        assertTrue(resStr.contains("               03 Flags--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("               03 SortBy--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("               03 ResultFields--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("               03 R-string--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("               03 SourceRequest--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("               03 Request."));
        assertTrue(resStr.contains("                   04 AppID PIC X(40)."));
        assertTrue(resStr.contains("                   04 Query PIC X(128)."));
        assertTrue(resStr.contains("                   04 CultureInfo PIC X(32)."));
        assertTrue(resStr.contains("                   04 SafeSearch PIC X(32)."));
        assertTrue(resStr.contains("                   04 Flags PIC X(32) OCCURS 1 TO 10 DEPENDING ON"));
        assertTrue(resStr.contains("                       Flags--C."));
        assertTrue(resStr.contains("                   04 Location."));
        assertTrue(resStr.contains("                       05 Latitude COMP-2."));
        assertTrue(resStr.contains("                       05 Longitude COMP-2."));
        assertTrue(resStr.contains("                       05 Radius COMP-2."));
        assertTrue(resStr.contains("                   04 Requests."));
        assertTrue(resStr.contains("                       05 SourceRequest OCCURS 0 TO 10 DEPENDING ON"));
        assertTrue(resStr.contains("                           SourceRequest--C."));
        assertTrue(resStr.contains("                           06 R-Source PIC X(32)."));
        assertTrue(resStr.contains("                           06 Offset PIC 9(9) COMP-5."));
        assertTrue(resStr.contains("                           06 R-Count PIC 9(9) COMP-5."));
        assertTrue(resStr.contains("                           06 FileType PIC X(32)."));
        assertTrue(resStr.contains("                           06 SortBy PIC X(32) OCCURS 1 TO 10 DEPENDING"));
        assertTrue(resStr.contains("                               ON SortBy--C."));
        assertTrue(resStr.contains("                           06 ResultFields PIC X(32) OCCURS 1 TO 10"));
        assertTrue(resStr.contains("                               DEPENDING ON ResultFields--C."));
        assertTrue(resStr.contains("                           06 SearchTagFilters."));
        assertTrue(resStr.contains("                               07 R-string PIC X(32) OCCURS 0 TO 10"));
        assertTrue(resStr.contains("                                   DEPENDING ON R-string--C."));
        assertTrue(resStr.contains("'MSNSEARC STARTING ==============================='"));
        assertTrue(resStr.contains("'MSNSEARC STOPPING ==============================='"));
    }
    /**
     * Generate the DFHWBCLI sample COBOL client and test it.
     * @throws Exception if generation fails
     */
    public void testCobolCicsDfhwbcliClientGeneration() throws Exception {

        CixsJaxwsService model = Samples.getJvmqueryWs();
        getParameters().put("cixsOperation", model.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        String resStr = genSource(model,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_DFHWBCLI_CLIENT_VLC_TEMPLATE,
                GEN_COBOL_DIR,
                model.getCixsOperations().get(0).getCicsProgramName() + ".cbl");

        String url = "http://" + CodeGenUtil.getLocalIPAddress() + ":8080/c2ws-jvmqueryWs/jvmqueryWsProxy";

        assertTrue(resStr.contains("       PROGRAM-ID. JVMQUERY."));
        assertTrue(resStr.contains("77  W00-SERVICE-URI               PIC X(" + url.length() + ") VALUE"));
        assertTrue(resStr.contains("'" + url + "'."));
        assertTrue(resStr.contains("'" + url + "'."));
        assertTrue(resStr.contains("           02 QueryJvm."));
        assertTrue(resStr.contains("               03 envVarNames--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("                   04 envVarNames PIC X(32) OCCURS 0 TO 10 DEPENDING ON"));
        assertTrue(resStr.contains("                   envVarNames--C."));
        assertTrue(resStr.contains("           02 QueryJvmResponse."));
        assertTrue(resStr.contains("               03 envVarValues--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("                   04 country PIC X(32)."));
        assertTrue(resStr.contains("                   04 currencySymbol PIC X(32)."));
        assertTrue(resStr.contains("                   04 envVarValues PIC X(32) OCCURS 0 TO 10 DEPENDING ON"));
        assertTrue(resStr.contains("                       envVarValues--C."));
        assertTrue(resStr.contains("                   04 formattedDate PIC X(32)."));
        assertTrue(resStr.contains("                   04 language PIC X(32)."));
        assertTrue(resStr.contains("'JVMQUERY STARTING ==============================='"));
        assertTrue(resStr.contains("'JVMQUERY STOPPING ==============================='"));
    }

    /**
     * Generate the WEB API sample COBOL client and test it.
     * @throws Exception if generation fails
     */
    public void testCobolCicsWebapiClientGeneration() throws Exception {

        CixsJaxwsService model = Samples.getJvmqueryWs();
        getParameters().put("cixsOperation", model.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        String resStr = genSource(model,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_WEBAPI_CLIENT_VLC_TEMPLATE,
                GEN_COBOL_DIR,
                model.getCixsOperations().get(0).getCicsProgramName() + ".cbl");

        String url = "http://" + CodeGenUtil.getLocalIPAddress() + ":8080/c2ws-jvmqueryWs/jvmqueryWsProxy";

        assertTrue(resStr.contains("       PROGRAM-ID. JVMQUERY."));
        assertTrue(resStr.contains("77  W00-SERVICE-URI               PIC X(" + url.length() + ") VALUE"));
        assertTrue(resStr.contains("'" + url + "'."));
        assertTrue(resStr.contains("           02 QueryJvm."));
        assertTrue(resStr.contains("               03 envVarNames--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("                   04 envVarNames PIC X(32) OCCURS 0 TO 10 DEPENDING ON"));
        assertTrue(resStr.contains("                   envVarNames--C."));
        assertTrue(resStr.contains("           02 QueryJvmResponse."));
        assertTrue(resStr.contains("               03 envVarValues--C PIC 9(9) BINARY."));
        assertTrue(resStr.contains("                   04 country PIC X(32)."));
        assertTrue(resStr.contains("                   04 currencySymbol PIC X(32)."));
        assertTrue(resStr.contains("                   04 envVarValues PIC X(32) OCCURS 0 TO 10 DEPENDING ON"));
        assertTrue(resStr.contains("                       envVarValues--C."));
        assertTrue(resStr.contains("                   04 formattedDate PIC X(32)."));
        assertTrue(resStr.contains("                   04 language PIC X(32)."));
        assertTrue(resStr.contains("'JVMQUERY STARTING ==============================='"));
        assertTrue(resStr.contains("'JVMQUERY STOPPING ==============================='"));
    }
}
