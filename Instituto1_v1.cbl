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
      **********************************
           03 Not-Alumno pic x(6).
           03 Not-Materia pic x(3).
      **********************************
           03 Not-Nota pic 9(2).

       FD  ACTAS.
       01  Act1-Reg.
           03 Act1-Tiporeg pic x.
      **********************************
           03 Act1-Acta pic 9(6).
      **********************************
           03 Act1-Fecha pic 9(8).
           03 Act1-Materia pic x(3).
       01  Act2-Reg.
           03 Act2-Tiporeg pic x.
      **********************************
           03 Act2-Alumno pic 9(6).
      **********************************
           03 Act2-Nota pic 9(2).

       FD  ALUMNOS.
       01  Alu-Reg.
      **********************************
           03 Alu-Codigo pic x(6).
      **********************************
           03 Alu-Nombre pic x(20).

       SD  ALUMNOS-sort.
       01  Alu-Sort-Reg.
           03 Alu-Sort-Legajo pic x(6).
           03 Alu-Sort-Nombre pic x(20).
           03 Alu-Sort-Promedio pic 9(2).


       WORKING-STORAGE SECTION.

       01  WS-FlagAlumno pic 9.
       01  WS-FlagNota pic 9.
       01  WS-FlagActa pic 9.
       01  WS-AcumNotas pic 9(5).
       01  WS-CantNotas pic 9(5).
       01  WS-PromedioNotas pic 9(2).
       01  WS-AcumNotas-total pic 9(5).
       01  WS-CantNotas-total pic 9(5).
       01  WS-FlagSort pic 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           SORT ALUMNOS-sort ASCENDING Alu-Sort-Legajo
           INPUT PROCEDURE IS input-procedure
           OUTPUT PROCEDURE IS output-procedure.

           STOP RUN.

       input-procedure.
           PERFORM 100-InicioGeneral
           PERFORM 200-LeerAlumnos
           PERFORM 300-LeerNotas
           PERFORM 400-LeerActas
           PERFORM UNTIL WS-FlagAlumno IS EQUAL 1
               PERFORM 150-InicioAcumuladores
               PERFORM 500-MoverNombre
               PERFORM 600-MoverLegajo
               PERFORM UNTIL Alu-Codigo IS NOT EQUAL Not-Alumno
                                        OR WS-FlagNota IS EQUAL 1
                   PERFORM 700-ProcesoNotas
                   PERFORM 800-CuentoNotas
                   PERFORM 300-LeerNotas
               END-PERFORM
               PERFORM 900-BuscarRegistro-D
               PERFORM UNTIL Act2-Alumno IS NOT EQUAL Alu-Codigo
                                        OR WS-FlagActa IS EQUAL 1
                                        OR Act2-Tiporeg is NOT EQUAL "D"
                   PERFORM 1000-ProcesoActas
                   PERFORM 800-CuentoNotas
                   PERFORM 400-LeerActas
               END-PERFORM
               PERFORM 1100-MoverNotas
               PERFORM 1200-GrabarSort
               PERFORM 200-LeerAlumnos
           END-PERFORM.
           PERFORM 1300-FinGeneral.
       output-procedure.
           PERFORM 1400-LeerArchivo-Sort.

      ******* RUTINAS INPUT-PROCEDURE *************************************

       100-InicioGeneral.
           OPEN INPUT NOTAS
           OPEN INPUT ALUMNOS
           OPEN INPUT ACTAS.

       150-InicioAcumuladores.
           move zero to WS-AcumNotas
           move zero to WS-CantNotas.

       200-LeerAlumnos.
           READ ALUMNOS AT END MOVE 1 TO WS-FlagAlumno.

       300-LeerNotas.
           READ NOTAS AT END MOVE 1 TO WS-FlagNota.

       400-LeerActas.
           READ ACTAS AT END MOVE 1 TO WS-FlagActa.

       500-MoverNombre.
           MOVE Alu-Nombre to Alu-Sort-Nombre.

       600-MoverLegajo.
           MOVE Alu-Codigo TO Alu-Sort-Legajo.

       700-ProcesoNotas.
           ADD Not-Nota to WS-AcumNotas.

       800-CuentoNotas.
           ADD 1 TO WS-CantNotas.

       900-BuscarRegistro-D.
           PERFORM UNTIL Act2-Tiporeg is EQUAL "D"
                                      OR WS-FlagActa IS EQUAL 1
           PERFORM 400-LeerActas
           END-PERFORM.

       1000-ProcesoActas.
           ADD Act2-Nota to WS-AcumNotas.

       1100-MoverNotas.
           ADD WS-AcumNotas to WS-AcumNotas-total
           ADD WS-CantNotas TO WS-CantNotas-total
           COMPUTE WS-PromedioNotas = WS-AcumNotas/WS-CantNotas
           MOVE WS-AcumNotas to WS-AcumNotas-total
           MOVE WS-CantNotas TO WS-CantNotas-total
           MOVE WS-PromedioNotas to Alu-Sort-Promedio.

       1200-GrabarSort.
           RELEASE Alu-Sort-Reg.

       1300-FinGeneral.
           close NOTAS
           close ALUMNOS
           close ACTAS.

       1400-LeerArchivo-Sort.
           RETURN ALUMNOS-sort at end move 1 to WS-FlagSort.


       END PROGRAM YOUR-PROGRAM-NAME.
