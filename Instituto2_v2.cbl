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

       01  WS-AuxDNI pic 9(8).
       01  WS-AuxAnio pic 9.
       01  WS-AuxDiv pic x.
       01  WS-AuxNombre pic x(40).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 100-InicioGeneral
           PERFORM 200-LeerCursos
           PERFORM 300-LeerNotas
           PERFORM 400-LeerAlumnos
           PERFORM 500-ArmarTabla

           SORT ALUMNOS-sort ASCENDING Alu-Sort-Promedio
           INPUT PROCEDURE IS input-procedure
           OUTPUT PROCEDURE IS output-procedure.

           PERFORM 1300-FinGeneral


           STOP RUN.


       100-InicioGeneral.
           OPEN INPUT CURSOS
           OPEN INPUT NOTAS
           OPEN INPUT ALUMNOS.

       200-LeerCursos.
           READ CURSOS AT END MOVE 1 TO WS-FlagCurso.
       300-LeerNotas.
           READ NOTAS AT END MOVE 1 TO WS-FlagNotas.
       400-LeerAlumnos.
           READ ALUMNOS AT END MOVE 1 TO WS-FlagAlumnos.

       500-ArmarTabla.
           PERFORM UNTIL WS-FlagCurso is EQUAL 1
               PERFORM VARYING i FROM 0 BY 1 UNTIL
                                                 Vec-Anio(i) = Cur-Anio
               END-PERFORM
               PERFORM VARYING j FROM 0 BY 1 UNTIL
                                                 Vec-Curso(j) = Cur-Div
               END-PERFORM
               move 1 to valor-celda(i,j)
               PERFORM 200-LeerCursos
           END-PERFORM.

       input-procedure.
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

       output-procedure.
           PERFORM 1000-LeerSort
           PERFORM 1100-BuscarEnTabla
           PERFORM UNTIL WS-FlagAlumnos-SORT IS EQUAL 1
                                     OR WS-VerdaderoCurso is EQUAL zero
               PERFORM 1200-ImprimoPantalla
               PERFORM 1000-LeerSort
               PERFORM 1100-BuscarEnTabla
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









       END PROGRAM YOUR-PROGRAM-NAME.
