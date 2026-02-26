FUNCTION z_invoic_mrm_target.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_IDOC_CONTROL) TYPE  EDIDC OPTIONAL
*"  TABLES
*"      T_IDOC_DATA STRUCTURE  EDIDD
*"      T_IDOC_STATUS_TEXT STRUCTURE  ZIDOCSTATTEXT
*"----------------------------------------------------------------------
  " Klasse instanziieren, Parameter übergeben
  DATA(lo_app) = NEW zcl_invoice_mrm_targe(
    ir_idoc_data          = REF #( t_idoc_data[] )
    ir_idoc_status_text   = REF #( t_idoc_status_text[] )
    ir_idoc_control       = REF #( is_idoc_control ) ).

  TRY.
      " IDoc-Verarbeitung
      lo_app->idoc_processing( ).

    CATCH zcx_invoice_mrm_example INTO DATA(lx_excp).
      lo_app->fill_status_table( lx_excp ).
  ENDTRY.


ENDFUNCTION.
