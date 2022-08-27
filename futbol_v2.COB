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
           SELECT NOVEDADES ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\novedades.txt"
           ORGANIZATION is line sequential.

           SELECT JUGADORES ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\jugadores.txt"
           ORGANIZATION is line sequential.

           SELECT EQUIPO ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\equipo.txt"
           ORGANIZATION is line sequential.

           SELECT EQUIPO-sort ASSIGN TO
           "D:\cobol\COBOL 32 - PARCIAL\alumnos-sort.txt"
           ORGANIZATION is line sequential.
      ******************************************************************
       DATA DIVISION.
           FILE SECTION.
       FD  NOVEDADES.
       01  Nov-reg.
           03 Nov-Equ pic 9(2).
           03 Nov-Cant_jug pic 9(2).
       01  Nov_Vec_Golead.
           03 Nov-Jug pic a(6).
           03 Nov-Goles pic 9(2).

       FD  JUGADORES.
       01  Jug_Cab-Reg.
           03 Jug_Cab_Tiporeg pic 9.
           03 Jug_Cab_Equ pic 9(2).
       01  Jug_Det-Reg.
           03 Jug_Det-Tiporeg pic x.
           03 Jug_Det-Jug pic 9(6).
           03 Jug_Det-Goles pic 9(8).

       FD  EQUIPO.
       01  Equ-Reg.
           03 Equ-Codigo pic 9(2).
           03 Equ-Nombre pic x(15).

       SD  EQUIPO-sort.
       01  EQUIPO-sort-Reg.
           03 Equ-sort-CodEqu pic 9(2).
           03 Equ-Sort-CodJug pic x(6).
           03 Equ-Sort-CantGol pic 9(8).


       WORKING-STORAGE SECTION.

       01  WS-FlagNovedades pic 9.
       01  WS-FlagJugadores pic 9.
       01  WS-FlagEquipo pic 9.
       01  WS-FlagSort pic 9.

       01  WS-CodigoEquipo pic 99.



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

           SORT EQUIPO-sort ASCENDING Equ-sort-CodEqu
           INPUT PROCEDURE IS input-procedure
           OUTPUT PROCEDURE IS output-procedure.

           STOP RUN.

       input-procedure.
           PERFORM 100-InicioGeneral
           PERFORM 200-LeerNovedades
           PERFORM 300-LeerJugadores
           PERFORM UNTIL WS-FlagNovedades IS EQUAL 1
                   PERFORM 400-MuevoNovedades-Sort
           END-PERFORM
           PERFORM UNTIL WS-FlagJugadores IS EQUAL 1
                   PERFORM 600-BuscarRegistro-1
                   MOVE Jug_Cab_Equ TO WS-CodigoEquipo
                   PERFORM 650-BuscarRegistro-2
                   PERFORM UNTIL Jug_Det-Tiporeg is NOT EQUAL 2
                                          or Jug_Det-Tiporeg IS EQUAL 1
                       MOVE WS-CodigoEquipo TO Equ-sort-CodEqu
                       MOVE Jug_Det-Jug to Equ-Sort-CodJug
                       MOVE Jug_Det-Goles to Equ-Sort-CantGol
                       PERFORM 500-Grabo-Sort
                       PERFORM 300-LeerJugadores
                   END-PERFORM
           END-PERFORM
           PERFORM 800-FinGeneral.
       output-procedure.
           PERFORM 900-InicioGeneral
           PERFORM 1000-LeerArchivo-Sort
           PERFORM 1100-LeerEQUIPO
           PERFORM 1150-ImprimoTitulo
           PERFORM until WS-FlagSort is EQUAL 1
               PERFORM 1200-BuscarNombre
               PERFORM 1300-InicializarVariables
               PERFORM UNTIL Equ-sort-CodEqu IS NOT EQUAL Equ-Codigo
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
           OPEN INPUT NOVEDADES
           OPEN INPUT JUGADORES.

       200-LeerNovedades.
           READ NOVEDADES AT END MOVE 1 TO WS-FlagNovedades.

       300-LeerJugadores.
           READ JUGADORES AT END MOVE 1 TO WS-FlagJugadores.

       400-MuevoNovedades-Sort.
           move Nov-Equ to WS-CodigoEquipo
           PERFORM 200-LeerNovedades
           PERFORM UNTIL Nov-Jug IS NUMERIC
                                       or WS-FlagNovedades IS EQUAL 1
               MOVE WS-CodigoEquipo TO Equ-sort-CodEqu
               MOVE Nov-Jug to Equ-Sort-CodJug
               MOVE Nov-Goles to Equ-Sort-CantGol
               PERFORM 500-Grabo-Sort
               PERFORM 200-LeerNovedades
           END-PERFORM.


       500-Grabo-Sort.
           RELEASE EQUIPO-sort-Reg.

       600-BuscarRegistro-1.
           PERFORM UNTIL Jug_Cab_Tiporeg is EQUAL 1
                                      OR WS-FlagJugadores IS EQUAL 1
           PERFORM 300-LeerJugadores
           END-PERFORM.

       650-BuscarRegistro-2.
           PERFORM UNTIL Jug_Cab_Tiporeg is EQUAL 2
                                      OR WS-FlagJugadores IS EQUAL 1
           PERFORM 300-LeerJugadores
           END-PERFORM.


       800-FinGeneral.
           close NOVEDADES
           close JUGADORES.

      ******* RUTINAS OUTPUT-PROCEDURE *************************************

       900-InicioGeneral.
           OPEN INPUT EQUIPO.

       1000-LeerArchivo-Sort.
           RETURN EQUIPO-sort at end move 1 to WS-FlagEquipo.

       1100-LeerEQUIPO.
           READ EQUIPO AT END MOVE 1 TO WS-FlagEquipo.

       1150-ImprimoTitulo.
           display Lin-titulo.

       1200-BuscarNombre.
           PERFORM UNTIL Equ-sort-CodEqu IS EQUAL Equ-Codigo
                                            or WS-FlagSort IS EQUAL 1
               PERFORM 900-InicioGeneral
           END-PERFORM
           MOVE Equ-Nombre to l-NombreAlumno
           MOVE Equ-Codigo to l-LegajoAlumno.

       1300-InicializarVariables.
           move zero to WS-AcumNotas
           move zero to WS-CantNotas.

       1400-ProcesoDatos.
           ADD Equ-Sort-CantGol TO WS-AcumNotas.

       1500-ImprimoDatos.
           MOVE WS-AcumNotas to l-PromedioNota
           Display Lin-alumno
           ADD WS-PromedioNotas TO WS-AcumNotas-total.


       1600-ImprimoFin.
           move WS-PromedioNotas-total to l-PromedioGeneral.
           DISPLAY Lin-fin.

       1700-FinGeneral.
           CLOSE EQUIPO.


       END PROGRAM YOUR-PROGRAM-NAME.
