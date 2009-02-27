       PROGRAM-ID. DPLARCHT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ZERO               PIC 9(05) VALUE ZERO.
       01  WS-ZEROS              PIC 9(05) VALUE ZEROS.
       01  WS-ZEROES             PIC 9(05) VALUE ZEROES.
       01  WS-SPACE              PIC X(05) VALUE SPACE.
       01  WS-SPACES             PIC X(05) VALUE SPACES.
       01  WS-HIGH-VALUE         PIC X(05) VALUE HIGH-VALUE.
       01  WS-HIGH-VALUES        PIC X(05) VALUE HIGH-VALUES.
       01  WS-LOW-VALUE          PIC X(05) VALUE LOW-VALUE.
       01  WS-LOW-VALUES         PIC X(05) VALUE LOW-VALUES.
       01  WS-QUOTE              PIC X(05) VALUE QUOTE.
       01  WS-QUOTES             PIC X(05) VALUE QUOTES.
       01  WS-NULL               PIC X(05) VALUE NULL.
       01  WS-NULLS              PIC X(05) VALUE NULLS.
       01  WS-STRING             PIC X(05) VALUE 'ABCDE'.
       01  WS-NUMERIC            PIC S9(05) VALUE -345.
       PROCEDURE DIVISION.
           GOBACK.
