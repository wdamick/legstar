package com.legstar.coxb.cob2trans.exe;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import com.legstar.coxb.cob2trans.AbstractCob2TransTester;
import com.legstar.coxb.cob2trans.Cob2TransGenerator;
import com.legstar.coxb.cob2trans.Cob2TransModel;

/**
 * Test the standalone executable.
 * 
 */
public class Cob2TransGeneratorMainTest extends AbstractCob2TransTester {

    /**
     * Test loading a non existing configuration.
     */
    public void testLoadModelNonExisting() {
        Cob2TransGeneratorMain main = new Cob2TransGeneratorMain();
        try {
            main.execute(new String[0]);
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " Input file or folder 'cobol' not found",
                    e.getMessage());
        }
    }

    /**
     * Test a complete generation for a single COBOL file.
     * 
     * @throws Exception if generation fails
     */
    public void testGenerate() throws Exception {
        Cob2TransGeneratorMain main = new Cob2TransGeneratorMain();
        main.execute(new String[] {
                "-c", "src/main/resources/conf/cob2trans.properties",
                "-i", COB_TEMP_DIR.getPath(),
                "-o", TARGET_DIR.getPath()
                });
        assertTrue(new File(_dirs.getDistDir(), _baseName
                + ".jar").exists());

    }

    /**
     * Perform a complete generation for several COBOL files.
     * Use a targetNamespace prefix to ensure each generated file set
     * gets a different namespace.
     * 
     * @throws Exception if generation fails
     */
    public void testMultipleGenerate() throws Exception {
        Cob2TransGeneratorMain main = new Cob2TransGeneratorMain();

        // Update configuration with a targetNamespace
        Properties config = new Properties();
        config.load(new FileInputStream(new File(
                "src/main/resources/conf/cob2trans.properties")));
        Cob2TransModel model = new Cob2TransModel(config);
        model.getCob2XsdModel().setTargetNamespace(
                "http://legstar.com/test/coxb/cob2trans/");
        config = model.toProperties();
        File tempConfigFile = new File(CONF_TEMP_DIR,
                "cob2trans.properties");
        config.store(new FileOutputStream(tempConfigFile), null);

        List < File > cobolFiles = new ArrayList < File >();
        // Create a series of COBOL files
        for (int i = 0; i < 3; i++) {
            cobolFiles.add(newTcobwvb());
        }

        main.execute(new String[] {
                "-c", tempConfigFile.getPath(),
                "-i", COB_TEMP_DIR.getPath(),
                "-o", TARGET_DIR.getPath()
                });

        for (File cobolFile : cobolFiles) {
            String baseName = Cob2TransGenerator.getBaseName(cobolFile);
            assertTrue(new File(TARGET_DIR,
                    baseName + "/dist/"
                            + baseName + ".jar").exists());
        }

    }
}
