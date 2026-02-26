CLASS zcl_invoice_mrm_stn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="de">Ruft IDOC_INPUT_INVOICE_MRM auf</p>
    "! <p>Verarbeitet IDOC-Daten für die Rechnungsfreigabe im MRM-Kontext</p>
    METHODS call_idoc_input_invoice_mrm
      IMPORTING
        it_edidc TYPE TABLE
        it_edid4 TYPE TABLE
      EXPORTING
        et_status TYPE TABLE
      RAISING
        zcx_invoice_mrm_example.

    "! <p class="shorttext synchronized" lang="de">Verarbeitet IDOC-Statussätze</p>
    "! <p>Liest EA066-Meldungen aus dem Application Log aus und bereitet die Statusmeldungen auf</p>
    METHODS process_idoc_status
      IMPORTING
        it_status TYPE TABLE
      EXPORTING
        et_processed_status TYPE TABLE
      RAISING
        cx_root.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_invoice_mrm_stn IMPLEMENTATION.

  METHOD call_idoc_input_invoice_mrm.
    "! Lokale Variablen
    DATA: lt_idoc_control TYPE TABLE OF edidc,
          lt_idoc_data    TYPE TABLE OF edid4,
          lt_idoc_status  TYPE TABLE OF edids.

    "! Input-Daten prüfen
    CHECK it_edidc IS NOT INITIAL AND it_edid4 IS NOT INITIAL.

    "! Eingabetabellen in lokale Variablen kopieren
    lt_idoc_control = it_edidc.
    lt_idoc_data = it_edid4.

    "! FM IDOC_INPUT_INVOICE_MRM aufrufen
    CALL FUNCTION 'IDOC_INPUT_INVOICE_MRM'
      TABLES
        idoc_control_records = lt_idoc_control
        idoc_data_records    = lt_idoc_data
        idoc_status          = lt_idoc_status
      EXCEPTIONS
        idoc_error           = 1
        OTHERS               = 2.

    "! Return-Code prüfen, Status zurückgeben oder Exception werfen
    IF sy-subrc = 0.
      "! Erfolgreicher Aufruf - Commit Work durchführen
      COMMIT WORK.
      et_status = lt_idoc_status.
    ELSE.
      "! Fehler aufgetreten - Exception werfen
      RAISE EXCEPTION TYPE zcx_invoice_mrm_example.
    ENDIF.

  ENDMETHOD.


  METHOD process_idoc_status.
    "! Lokale Variablen
    DATA: ls_status         TYPE edids,
          ls_processed      TYPE edids,
          lt_bal_logs       TYPE bal_t_logh,
          lt_bal_messages   TYPE bal_t_msgh,
          lv_counter        TYPE int4,
          lv_log_handle     TYPE balloghndl.

    "! Status-Tabellen initialisieren
    CLEAR et_processed_status.

    "! Über alle Statussätze iterieren
    LOOP AT it_status INTO ls_status.

      "! Neuen Statussatz vorbereiten
      CLEAR ls_processed.
      ls_processed-docnum = ls_status-docnum.
      ls_processed-status = ls_status-status.
      ls_processed-mandt = ls_status-mandt.

      "! Prüfe auf EA066 - Fehler im Application Log
      IF ( ls_status-status = 'EA' OR ls_status-status = 'E' ) AND ls_status-appl_log IS NOT INITIAL.

        "! Log Handle aus APPL_LOG auslesen
        lv_log_handle = ls_status-appl_log.

        "! Versuche, Messages aus Application Log auszulesen mit BAL_DB_LOAD
        CALL FUNCTION 'BAL_DB_LOAD'
          EXPORTING
            i_t_log_handle = lt_bal_logs
          EXCEPTIONS
            OTHERS         = 1.

        IF sy-subrc = 0.

          "! BAL_LOG_MSG_READ zum Auslesen der Messages verwenden
          CALL FUNCTION 'BAL_LOG_MSG_READ'
            EXPORTING
              i_t_log_handle = lt_bal_logs
            IMPORTING
              e_t_messages   = lt_bal_messages
            EXCEPTIONS
              OTHERS         = 2.

          "! Meldungen in den Status übernehmen
          IF sy-subrc = 0 AND lt_bal_messages IS NOT INITIAL.
            "! Erste Meldung auslesen
            READ TABLE lt_bal_messages INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_msg>).
            IF sy-subrc = 0.
              ls_processed-status = 'E'.
            ENDIF.
          ENDIF.

        ENDIF.

      ENDIF.

      "! Verarbeiteten Status zur Ausgabetabelle hinzufügen
      APPEND ls_processed TO et_processed_status.

      ADD 1 TO lv_counter.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
