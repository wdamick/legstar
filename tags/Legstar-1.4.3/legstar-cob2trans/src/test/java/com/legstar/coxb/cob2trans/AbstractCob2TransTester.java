package com.legstar.coxb.cob2trans;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;

/**
 * Common test methods.
 * 
 */
public abstract class AbstractCob2TransTester extends TestCase {

    /** Sample TCOCBWVB. */
    public static final String TCOBWVB_STRUCTURE =
            "       01  CUSTOMER-DATA.\n"
                    + "           05 CUSTOMER-ID             PIC 9(6).\n"
                    + "           05 PERSONAL-DATA.\n"
                    + "              10 CUSTOMER-NAME        PIC X(20).\n"
                    + "              10 CUSTOMER-ADDRESS     PIC X(20).\n"
                    + "              10 CUSTOMER-PHONE       PIC X(8).\n"
                    + "           05 TRANSACTIONS.\n"
                    + "              10 TRANSACTION-NBR      PIC 9(9) COMP.\n"
                    + "              10 TRANSACTION OCCURS 0 TO 5\n"
                    + "                 DEPENDING ON TRANSACTION-NBR.\n "
                    + "                 15 LAST-TRANS-DATE         PIC X(8).\n"
                    + "                 15 FILLER REDEFINES LAST-TRANS-DATE.\n"
                    + "                    20 LAST-TRANS-DAY       PIC X(2).\n"
                    + "                    20 FILLER               PIC X.\n"
                    + "                    20 LAST-TRANS-MONTH     PIC X(2).\n"
                    + "                    20 FILLER               PIC X.\n"
                    + "                    20 LAST-TRANS-YEAR      PIC X(2).\n"
                    + "                 15 LAST-TRANS-AMOUNT       PIC S9(13)V99 COMP-3.\n"
                    + "                 15 LAST-TRANS-COMMENT      PIC X(9).\n";

    /** Destination for testing. */
    public static final File TARGET_DIR = new File("target/gen");

    /** Temporary location for COBOL test files. */
    public static final File COB_TEMP_DIR = new File("target/cobol");

    /** Temporary location for configuration files. */
    public static final File CONF_TEMP_DIR = new File("target/conf");

    /** Options for generation. */
    public Cob2TransModel _context;

    /** A COBOL file ready for unit testing. */
    public File _cobolFile;

    /** Artifacts group name for the COBOL file. */
    public String _baseName;

    /** Target folders for generation. */
    public Cob2TransDirs _dirs;

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        FileUtils.deleteQuietly(COB_TEMP_DIR);
        FileUtils.forceMkdir(COB_TEMP_DIR);

        FileUtils.deleteQuietly(CONF_TEMP_DIR);
        FileUtils.forceMkdir(CONF_TEMP_DIR);

        FileUtils.deleteQuietly(TARGET_DIR);
        FileUtils.forceMkdir(TARGET_DIR);

        _context = new Cob2TransModel();
        _context.setCleanFolders(true);

        _cobolFile = newTcobwvb();
        _baseName = Cob2TransGenerator.getBaseName(_cobolFile);
        _dirs = Cob2TransGenerator.prepareTarget(TARGET_DIR, _baseName,
                _context);
    }

    /**
     * @return the TCOBWVB sample
     */
    public File newTcobwvb() {
        return toFile(TCOBWVB_STRUCTURE);
    }

    /**
     * Dumps content to a temporary file.
     * 
     * @param content some COBOL data item descriptions
     * @return a temporary file with the content
     */
    protected File toFile(final String content) {
        try {
            File cobolFile = File.createTempFile("test", ".cbl", COB_TEMP_DIR);
            cobolFile.deleteOnExit();
            FileUtils.writeStringToFile(cobolFile, content);
            return cobolFile;
        } catch (IOException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
        return null;
    }

    /**
     * Checks that a file contains all character strings.
     * 
     * @param file the file to check
     * @param strings the character strings to look for
     */
    public void assertFileContainsAll(final File file, final String[] strings) {
        try {
            String content = FileUtils.readFileToString(file);
            assertStringContainsAll(content, strings);
        } catch (IOException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }

    }

    /**
     * Checks that a character string contains all character strings.
     * 
     * @param str the string to check
     * @param strings the character strings to look for
     */
    public void assertStringContainsAll(final String str, final String[] strings) {
        for (String s : strings) {
            assertTrue(str.contains(s));
        }
    }

    /**
     * Checks that a character string contains at least one character string.
     * 
     * @param str the string to check
     * @param strings the character strings to look for
     */
    public void assertStringContainsoneOf(final String str,
            final String[] strings) {
        for (String s : strings) {
            if (str.contains(s)) {
                return;
            }
        }
        fail("String " + str + " does not contains any of " + strings);
    }
}
