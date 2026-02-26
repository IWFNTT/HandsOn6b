FUNCTION z_invoice_mrm_copilot_stn.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_IDOC_CONT STRUCTURE  EDIDC
*"      T_EDIDD STRUCTURE  EDIDD
*"      T_IDOC_STATUS_TEXT STRUCTURE  ZIDOCSTATTEXT
*"----------------------------------------------------------------------



*  DATA(lo_processor) = NEW zcl_invoice_mrm_stn( ).
*
*  TRY.
*      lo_processor->call_idoc_input_invoice_mrm(
*        EXPORTING
*          it_edidc  = t_idoc_contrl
*          it_edid4  =
**        IMPORTING
**          et_status =
*      ).
**      CATCH zcx_invoice_mrm_example.(
*          EXPORTING
*            it_idoc_contrl = t_idoc_contrl
*            it_idoc_data   = t_idoc_data
*          IMPORTING
*            et_status_text = t_idoc_status_text ).
*
*    CATCH cx_root INTO DATA(lx_err).
*      CLEAR t_idoc_status_text.
*      t_idoc_status_text-msgid = 'ZRFC'.
*      t_idoc_status_text-msgno = '999'.
*      t_idoc_status_text-msgty = 'E'.
*      t_idoc_status_text-msgv1 = lx_err->get_text( ).
*  ENDTRY.



ENDFUNCTION.
