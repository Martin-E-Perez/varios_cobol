      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
      ******************************************************************
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
               SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
           SELECT NOTAS ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\notas.txt"
           ORGANIZATION is line sequential.

           SELECT ACTAS ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\actas.txt"
           ORGANIZATION is line sequential.

           SELECT ALUMNOS ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\alumnos.txt"
           ORGANIZATION is line sequential.

           SELECT ALUMNOS-sort ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\alumnos-sort.txt"
           ORGANIZATION is line sequential.
      ******************************************************************
       DATA DIVISION.
           FILE SECTION.
       FD  NOTAS.
       01  Not-reg.
           03 Not-Acta pic 9(6).
           03 Not-Fecha pic 9(8).
           03 Not-Alumno pic x(6).
           03 Not-Materia pic x(3).
           03 Not-Nota pic 9(2).

       FD  ACTAS.
       01  Act1-Reg.
           03 Act1-Tiporeg pic x.
           03 Act1-Acta pic 9(6).
           03 Act1-Fecha pic 9(8).
           03 Act1-Materia pic x(3).
       01  Act2-Reg.
           03 Act2-Tiporeg pic x.
           03 Act2-Alumno pic 9(6).
           03 Act2-Nota pic 9(2).

       FD  ALUMNOS.
       01  Alu-Reg.
           03 Alu-Codigo pic x(6).
           03 Alu-Nombre pic x(20).

       SD  ALUMNOS-sort.
       01  Alu-Sort-Reg.
           03 Alu-Sort-Legajo pic x(6).
           03 Alu-Sort-Promedio pic 9(2).


       WORKING-STORAGE SECTION.

       01  WS-FlagAlumno pic 9.
       01  WS-FlagNota pic 9.
       01  WS-FlagActa pic 9.
       01  WS-FlagSort pic 9.
       01  WS-legajoAnterior pic x(6).

       01  WS-AcumNotas pic 9(5).
       01  WS-CantNotas pic 9(5).
       01  WS-PromedioNotas pic 9(2).


       01  WS-AcumNotas-total pic 9(5).
       01  WS-CantNotas-total pic 9(5).
       01  WS-PromedioNotas-total pic 9(2).


       01  Lin-titulo.
           03 filler pic x(6) value "legajo".
           03 filler PIC x(4) VALUE SPACE.
           03 filler PIC X(20) value "Nombre Alumno".
           03 filler PIC x(5) VALUE SPACE.
           03 filler PIC X(20) value "Promedio Nota".

       01  Lin-alumno.
           03 l-LegajoAlumno pic zzz9.
           03 filler PIC x(5) VALUE SPACE.
           03 l-NombreAlumno pic X(20).
           03 filler PIC x(10) VALUE SPACE.
           03 l-PromedioNota pic z9.

       01  Lin-fin.
           03 filler pic x(16) value "PromedioGeneral".
           03 l-PromedioGeneral pic z9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           SORT ALUMNOS-sort ASCENDING Alu-Sort-Legajo
           INPUT PROCEDURE IS input-procedure
           OUTPUT PROCEDURE IS output-procedure.

           STOP RUN.

       input-procedure.
           PERFORM 100-InicioGeneral
           PERFORM 200-LeerNotas
           PERFORM 300-LeerActas
           PERFORM UNTIL WS-FlagNota IS EQUAL 1
                   PERFORM 400-MuevoNotas-Sort
                   PERFORM 500-Grabo-Sort
                   PERFORM 200-LeerNotas
           END-PERFORM
           PERFORM UNTIL WS-FlagActa IS EQUAL 1
                   PERFORM 600-BuscarRegistro-D
                   PERFORM UNTIL Act2-Tiporeg is NOT EQUAL "D"
                                             or WS-FlagActa IS EQUAL 1
                       PERFORM 700-MuevoActa-Sort
                       PERFORM 500-Grabo-Sort
                       PERFORM 300-LeerActas
                   END-PERFORM
           END-PERFORM
           PERFORM 800-FinGeneral.
       output-procedure.
           PERFORM 900-InicioGeneral
           PERFORM 1000-LeerArchivo-Sort
           PERFORM 1100-LeerAlumno
           PERFORM 1150-ImprimoTitulo
           PERFORM until WS-FlagSort is EQUAL 1
               PERFORM 1200-BuscarNombre
               PERFORM 1300-InicializarVariables
               PERFORM UNTIL Alu-Codigo IS NOT EQUAL Alu-Sort-Legajo
                                           or WS-FlagSort is EQUAL 1
                   PERFORM 1400-ProcesoDatos
                   PERFORM 1000-LeerArchivo-Sort
               END-PERFORM
               PERFORM 1500-ImprimoDatos
           END-PERFORM
           PERFORM 1600-ImprimoFin.
           PERFORM 1700-FinGeneral.

      ******* RUTINAS INPUT-PROCEDURE *************************************

       100-InicioGeneral.
           OPEN INPUT NOTAS
           OPEN INPUT ACTAS.

       200-LeerNotas.
           READ NOTAS AT END MOVE 1 TO WS-FlagNota.

       300-LeerActas.
           READ ACTAS AT END MOVE 1 TO WS-FlagActa.

       400-MuevoNotas-Sort.
           move Not-Alumno to Alu-Sort-Legajo
           move Not-Nota to Alu-Sort-Promedio.

       500-Grabo-Sort.
           RELEASE Alu-Sort-Reg.

       600-BuscarRegistro-D.
           PERFORM UNTIL Act2-Tiporeg is EQUAL "D"
                                      OR WS-FlagActa IS EQUAL 1
           PERFORM 300-LeerActas
           END-PERFORM.

       700-MuevoActa-Sort.
           move Act2-Alumno to Alu-Sort-Legajo
           move Act2-Nota to Alu-Sort-Promedio.

       800-FinGeneral.
           close NOTAS
           close ACTAS.

      ******* RUTINAS OUTPUT-PROCEDURE *************************************

       900-InicioGeneral.
           OPEN INPUT ALUMNOS.

       1000-LeerArchivo-Sort.
           RETURN ALUMNOS-sort at end move 1 to WS-FlagSort.

       1100-LeerAlumno.
           READ NOTAS AT END MOVE 1 TO WS-FlagNota.

       1150-ImprimoTitulo.
           display Lin-titulo.

       1200-BuscarNombre.
           PERFORM UNTIL Alu-Sort-Legajo IS EQUAL Alu-Codigo
                                   or WS-FlagSort IS EQUAL 1
               PERFORM 900-InicioGeneral
           END-PERFORM
           MOVE Alu-Nombre to l-NombreAlumno
           MOVE Alu-Codigo to l-LegajoAlumno.

       1300-InicializarVariables.
           move zero to WS-AcumNotas
           move zero to WS-CantNotas.

       1400-ProcesoDatos.
           ADD Alu-Sort-Promedio TO WS-AcumNotas
           ADD 1 TO WS-CantNotas.

       1500-ImprimoDatos.
           COMPUTE WS-PromedioNotas = WS-AcumNotas / WS-CantNotas
           IF WS-PromedioNotas >= 8
               MOVE WS-PromedioNotas to l-PromedioNota
               Display Lin-alumno
               ADD WS-PromedioNotas TO WS-AcumNotas-total
               ADD 1 TO WS-CantNotas-total
           END-IF.

       1600-ImprimoFin.
           COMPUTE WS-PromedioNotas-total =
                               WS-AcumNotas-total / WS-CantNotas-total
           move WS-PromedioNotas-total to l-PromedioGeneral.
           DISPLAY Lin-fin.

       1700-FinGeneral.
           CLOSE ALUMNOS.


       END PROGRAM YOUR-PROGRAM-NAME.
