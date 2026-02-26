*======================================================================*
*  CR / ID            : DEV-925
*  Title              : Z-RFC-Baustein für kred. Rechnungen (AutoPOM)​
*  Responsible        : M. Pohlmann  / NTT Data Business Solutions AG
*======================================================================
*  Change history
*  Date       | User        | CR / ID / Changes
* -----------  ------------  -------------------------------------------
*  16.12.2024   CAL           initial implementation
*======================================================================*
CLASS zcl_invoice_mrm_targe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_idoc_data TYPE STANDARD TABLE OF edidd WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_idoc_status_text TYPE STANDARD TABLE OF zidocstattext WITH NON-UNIQUE DEFAULT KEY .
*    TYPES:
*      tt_material_qualifier TYPE STANDARD TABLE OF zposmat WITH NON-UNIQUE DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !ir_idoc_data        TYPE REF TO data OPTIONAL
        !ir_idoc_status_text TYPE REF TO data OPTIONAL
        !ir_idoc_control     TYPE REF TO edidc OPTIONAL .
    METHODS idoc_processing
      RAISING
        zcx_invoice_mrm_example .
    METHODS fill_status_table
      IMPORTING
        !ix_excp TYPE REF TO cx_root .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_idoc_data TYPE REF TO tt_idoc_data .
    DATA mr_idoc_status_text TYPE REF TO tt_idoc_status_text .
    DATA mr_idoc_control TYPE REF TO edidc .

    METHODS read_appl_log
      IMPORTING
        !it_idoc_status TYPE t_idoc_status
      EXPORTING
        !et_app_log     TYPE balm_t .
ENDCLASS.



CLASS zcl_invoice_mrm_targe IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS <lt_idoc_status_text> TYPE tt_idoc_status_text.
    FIELD-SYMBOLS <lt_idoc_data> TYPE tt_idoc_data.

    ASSIGN ir_idoc_status_text->* TO <lt_idoc_status_text>.

    GET REFERENCE OF <lt_idoc_status_text> INTO mr_idoc_status_text.

    mr_idoc_control = ir_idoc_control.

    ASSIGN ir_idoc_data->* TO <lt_idoc_data>.

    GET REFERENCE OF <lt_idoc_data> INTO mr_idoc_data.

  ENDMETHOD.


  METHOD fill_status_table.

    TRY.
        DATA(lx_excp) = CAST if_t100_dyn_msg( ix_excp ).
        DATA(lx_925) = CAST zcx_invoice_mrm_example( ix_excp ).
      CATCH cx_sy_move_cast_error.
        IF lx_excp IS NOT BOUND.
          RETURN.
        ENDIF.
        IF lx_925 IS BOUND.
          DATA(lf_segnum) = lx_925->mf_segnum.
        ENDIF.
    ENDTRY.

    APPEND VALUE #(
      docnum = mr_idoc_control->docnum
      status = SWITCH #( lx_excp->msgty WHEN 'E' OR 'A' OR 'X' THEN '51' ELSE '53' )
      msgty  = lx_excp->msgty
      msgid  = lx_excp->if_t100_message~t100key-msgid
      msgno  = lx_excp->if_t100_message~t100key-msgno
      msgv1  = lx_excp->msgv1
      msgv2  = lx_excp->msgv2
      msgv3  = lx_excp->msgv3
      msgv4  = lx_excp->msgv4
      segnum = lf_segnum
      uname  = sy-uname
      repid  = sy-repid
      mess   = ix_excp->get_text( )
      logdat = sy-datum
      logtim = sy-uzeit
    ) TO mr_idoc_status_text->*.

  ENDMETHOD.


  METHOD idoc_processing.
*Data deklarations
    DATA lt_idoc_contrl           TYPE STANDARD TABLE OF edidc.
    DATA lt_idoc_status           TYPE STANDARD TABLE OF bdidocstat.
    DATA lt_return_variables      TYPE STANDARD TABLE OF bdwfretvar.
    DATA lt_serialization_info    TYPE STANDARD TABLE OF bdi_ser.
    DATA lt_app_log               TYPE balm_t.

*Fill Control
    lt_idoc_contrl = VALUE #( ( mr_idoc_control->* ) ).

*IDOC_INPUT_INVOIC_MRM
    CALL FUNCTION 'IDOC_INPUT_INVOIC_MRM' "#EC CI_USAGE_OK[2522971]
      EXPORTING
        input_method          = VALUE bdwfap_par-inputmethd( )
        mass_processing       = VALUE bdwfap_par-mass_proc( )
      TABLES
        idoc_contrl           = lt_idoc_contrl
        idoc_data             = mr_idoc_data->*
        idoc_status           = lt_idoc_status
        return_variables      = lt_return_variables
        serialization_info    = lt_serialization_info
      EXCEPTIONS
        wrong_function_called = 1
        OTHERS                = 2.
*Return
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.

      DATA(lf_docnum) = lt_idoc_contrl[ 1 ]-docnum.

      CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
        EXPORTING
          idoc_number               = lf_docnum
*         IDOC_OPENED_FLAG          = ''
*         NO_DEQUEUE_FLAG           = 'X'
*       IMPORTING
*         IDOC_CONTROL              =
        TABLES
          idoc_status               = lt_idoc_status
        EXCEPTIONS
          idoc_foreign_lock         = 1
          idoc_not_found            = 2
          idoc_status_records_empty = 3
          idoc_status_invalid       = 4
          db_error                  = 5
          OTHERS                    = 6.

      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

    ELSE.

      RAISE EXCEPTION TYPE zcx_invoice_mrm_example
         MESSAGE ID zcx_invoice_mrm_example=>c_wrong_function-msgid
            TYPE 'E'
          NUMBER zcx_invoice_mrm_example=>c_wrong_function-msgno
            WITH 'IDOC_INPUT_INVOIC_MRM'.
    ENDIF.
*APPL_LOG_READ_DB_WITH_LOGNO
    CALL METHOD me->read_appl_log
      EXPORTING
        it_idoc_status = lt_idoc_status
      IMPORTING
        et_app_log     = lt_app_log.

    DATA lf_msg TYPE text100.

*APPL_LOG_READ_DB_WITH_LOGNO
    LOOP AT lt_idoc_status ASSIGNING FIELD-SYMBOL(<ls_idoc_status>).

      DATA(ls_status_text)  = CORRESPONDING zidocstattext( <ls_idoc_status> ).
*Read APP Log
*      READ TABLE lt_app_log INTO DATA(ls_app_log)       "#EC CI_SORTSEQ
*      WITH KEY lognumber = <ls_idoc_status>-appl_log.

      LOOP AT lt_app_log INTO DATA(ls_app_log) WHERE lognumber = <ls_idoc_status>-appl_log.

        MOVE-CORRESPONDING ls_app_log TO ls_status_text.

*Read Text
        ls_status_text-logdat = sy-datum.
        ls_status_text-logtim = sy-uzeit.

        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = ls_status_text-msgid
*           lang      = sy-langu
            no        = ls_status_text-msgno
            v1        = ls_status_text-msgv1
            v2        = ls_status_text-msgv2
            v3        = ls_status_text-msgv3
            v4        = ls_status_text-msgv4
          IMPORTING
            msg       = ls_status_text-mess
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
          CLEAR lf_msg.
        ENDIF.

        APPEND ls_status_text TO mr_idoc_status_text->*.

      ENDLOOP.

      CHECK sy-subrc NE 0.

      MOVE-CORRESPONDING <ls_idoc_status> TO ls_status_text.

*      *Read Text
      ls_status_text-logdat = sy-datum.
      ls_status_text-logtim = sy-uzeit.

      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = ls_status_text-msgid
*         lang      = sy-langu
          no        = ls_status_text-msgno
          v1        = ls_status_text-msgv1
          v2        = ls_status_text-msgv2
          v3        = ls_status_text-msgv3
          v4        = ls_status_text-msgv4
        IMPORTING
          msg       = ls_status_text-mess
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
        CLEAR lf_msg.
      ENDIF.

      APPEND ls_status_text TO mr_idoc_status_text->*.

    ENDLOOP.


  ENDMETHOD.


  METHOD read_appl_log.

    DATA:
      lt_LOGNUMBERS TYPE szal_lognumbers,
      lt_MESSAGES   TYPE STANDARD TABLE OF balm.

    CLEAR et_app_log[].

*LOGNUMBERS
    LOOP AT it_idoc_status INTO DATA(ls_STATUS) WHERE appl_log NE space .
      APPEND ls_STATUS-appl_log TO lt_LOGNUMBERS[].
    ENDLOOP.
*APPL_LOG_READ_DB_WITH_LOGNO
    CALL FUNCTION 'APPL_LOG_READ_DB_WITH_LOGNO'
      TABLES
        lognumbers = lt_LOGNUMBERS
        messages   = lt_MESSAGES[].

    et_app_log[] =  lt_MESSAGES[].


  ENDMETHOD.
ENDCLASS.
