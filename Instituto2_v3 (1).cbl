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

           SELECT CURSOS ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\cursos.txt"
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
       01  Not1-reg.
           03 Not1-Tiporeg pic 9.
           03 Not1-Dni pic 9(8).
           03 Not1-Anio pic 9.
           03 Not1-Div pic x.
       01  Not2-reg.
           03 Not2-Tiporeg pic 9.
           03 Not2-Materia pic 9(4).
           03 Not2-Nota pic 99v99.

       FD  CURSOS.
       01  Cur-Reg.
           03 Cur-Anio pic 9.
           03 Cur-Div pic x.

       FD  ALUMNOS.
       01  Alu-Reg.
           03 Alu-Dni pic x(8).
           03 Alu-Nombre pic x(40).

       SD  ALUMNOS-sort.
       01  Alu-Sort-Reg.
           03 Alu-Sort-Dni pic 9(8).
           03 Alu-Sort-Nombre pic x(40).
           03 Alu-Sort-Promedio pic 99v99.
           03 Alu-Sort-Anio pic 9.
           03 Alu-Sort-Div pic x.


       WORKING-STORAGE SECTION.
        01 AnioElectivo.
           03 filler pic x value "1".
           03 filler pic x value "2".
           03 filler pic x value "3".
           03 filler pic x value "4".
           03 filler pic x value "5".

       01  Vec-Anio redefines AnioElectivo OCCURS 5 times.
           03 Anio-Cod pic x.

       01  Curso.
           03 filler pic x value "A".
           03 filler pic x value "B".
           03 filler pic x value "C".
           03 filler pic x value "D".
           03 filler pic x value "E".
           03 filler pic x value "F".
           03 filler pic x value "G".
           03 filler pic x value "H".

       01  Vec-Curso redefines Curso OCCURS 8 times.
           03 Curso-Cod pic x.

       01  i pic 9.
       01  j pic 9.


       01  Tabla-anio-div.
           03 vec-div-fila OCCURS 8 times.
               05 vec-anio-colum OCCURS 5 times.
                   07 valor-celda pic 9 value zero.

       01  WS-FlagCurso PIC 9.
       01  WS-FlagNotas PIC 9.
       01  WS-FlagAlumnos PIC 9.
       01  WS-FlagAlumnos-Sort PIC 9.

       01  WS-AuxDNI pic 9(8).
       01  WS-AuxAnio pic 9.
       01  WS-AuxDiv pic x.
       01  WS-AuxNombre pic x(40).

       01  WS-AcumNotas pic 9999v99.
       01  WS-ContNotas pic 999.
       01  WS-Promedio pic 99v99.

       01  WS-VerdaderoCurso PIC 9.

       01  Lin-alumno.
           03 l-NombreAlumno pic X(40).
           03 filler PIC x(10) VALUE SPACE.
           03 l-PromedioNota pic z9.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           SORT ALUMNOS-sort ASCENDING Alu-Sort-Promedio
           INPUT PROCEDURE IS input-procedure
           OUTPUT PROCEDURE IS output-procedure.

           STOP RUN.

       input-procedure.
           PERFORM 100-InicioCursos
           PERFORM 110-InicioNotas
           PERFORM 200-LeerCursos
           PERFORM 500-ArmarTabla-Cursos
           PERFORM UNTIL WS-FlagNotas IS EQUAL 1
               PERFORM 600-BuscarCabezera-1
               PERFORM 650-GuardoDatos
               PERFORM 1500-BUSCAR_NOMBRE
               PERFORM 700-BuscarCabezera-2
               PERFORM until WS-FlagNotas IS EQUAL 1
                                     or Not2-Tiporeg is not EQUAL 2
                   PERFORM 800-SumarNotas
                   PERFORM 300-LeerNotas
               END-PERFORM
               PERFORM 900-GrabarSort
           END-PERFORM.
           PERFORM 1300-FinGeneral.

       output-procedure.
           PERFORM 120-InicioAlumnos
           PERFORM 1000-LeerSort
           PERFORM 1100-BuscarEnTabla
           PERFORM UNTIL WS-FlagAlumnos-SORT IS EQUAL 1
               IF valor-celda(i,j) is EQUAL 1
                   PERFORM 1200-ImprimoPantalla
               END-IF
               PERFORM 1000-LeerSort
               PERFORM 1100-BuscarEnTabla
           END-PERFORM.
           PERFORM 1400-FinAlumnos.

      *************************************************************************
      ***************************** input-procedure ***************************

       100-InicioCursos.
           OPEN INPUT CURSOS.

       110-InicioNotas.
           OPEN INPUT NOTAS.


       120-InicioAlumnos.
           OPEN INPUT ALUMNOS.

       200-LeerCursos.
           READ CURSOS AT END MOVE 1 TO WS-FlagCurso.
       300-LeerNotas.
           READ NOTAS AT END MOVE 1 TO WS-FlagNotas.
       400-LeerAlumnos.
           READ ALUMNOS AT END MOVE 1 TO WS-FlagAlumnos.

       500-ArmarTabla-Cursos.
           PERFORM UNTIL WS-FlagCurso is EQUAL 1
               PERFORM VARYING i FROM 0 BY 1 UNTIL
                                                 Vec-Anio(i) = Cur-Anio
                                                 OR i > 5
               END-PERFORM
               PERFORM VARYING j FROM 0 BY 1 UNTIL
                                                 Vec-Curso(j) = Cur-Div
                                                 OR j > 8
               END-PERFORM
               move 1 to valor-celda(i,j)
               PERFORM 200-LeerCursos
           END-PERFORM.

       600-BuscarCabezera-1.
           PERFORM until WS-FlagNotas is EQUAL 1
                                            or Not1-Tiporeg is EQUAL 1
           PERFORM 300-LeerNotas
           END-PERFORM.
       650-GuardoDatos.
           move Not1-Dni to WS-AuxDNI
           move Not1-Anio to WS-AuxAnio
           move Not1-Div to WS-AuxDiv.

       1500-BUSCAR_NOMBRE.
           PERFORM UNTIL WS-FlagAlumnos is EQUAL 1 or
                                       Alu-Dni is EQUAL Not1-Dni
                 PERFORM 400-LeerAlumnos
           END-PERFORM
           move Alu-Nombre to WS-AuxNombre.

       700-BuscarCabezera-2.
           PERFORM until WS-FlagNotas is EQUAL 1
                                            or Not1-Tiporeg is EQUAL 2
           PERFORM 300-LeerNotas
           END-PERFORM.

       800-SumarNotas.
           add Not2-Nota to WS-AcumNotas
           add 1 to WS-ContNotas.

       900-GrabarSort.
           move WS-AuxDNI TO Alu-Sort-Dni
           MOVE WS-AuxAnio TO Alu-Sort-Anio
           MOVE WS-AuxNombre TO Alu-Sort-Nombre
           MOVE WS-AuxDiv TO Alu-Sort-Div
           COMPUTE WS-Promedio = WS-AcumNotas / WS-ContNotas
           MOVE WS-Promedio TO Alu-Sort-Promedio

           RELEASE Alu-Sort-Reg.

       1300-FinGeneral.
           close NOTAS
           CLOSE CURSOS.
      ************************************************************************
      ***************************** output-procedure ***************************

       1000-LeerSort.
            RETURN ALUMNOS-sort AT END MOVE 1 TO WS-FlagAlumnos-Sort.

       1100-BuscarEnTabla.

           PERFORM VARYING i FROM 0 BY 1
                                      UNTIL Vec-Anio(i) = Alu-Sort-Anio
                                      OR i > 5
           END-PERFORM

           PERFORM VARYING j FROM 0 BY 1
                                      UNTIL Vec-Curso(j) = Alu-Sort-Div
                                      or j > 8
           END-PERFORM.

       1200-ImprimoPantalla.
           MOVE Alu-Sort-Nombre TO l-NombreAlumno
           MOVE Alu-Sort-Promedio to l-PromedioNota
           display Lin-alumno.

       1400-FinAlumnos.
           close ALUMNOS.


       END PROGRAM YOUR-PROGRAM-NAME.
