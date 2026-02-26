*======================================================================*
*  CR / ID            : DEV-790 / SPS-195
*  Title              : EDI - Z-RFC-Baustein für Kundenauftragsanlage
*  Responsible        : Robert Heinemann / NTT Data Business Solutions AG
*======================================================================
*  Change history
*  Date       | User        | CR / ID / Changes
* -----------  ------------  -------------------------------------------
*  30.05.2024   MSZ           initial implementation
*  dd.mm.20yy   <Username>    CR nnn / IE-xxx: .......
*
*======================================================================*
CLASS ZCX_invoice_MRM_EXAMPLE DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    CONSTANTS:
      BEGIN OF c_no_items,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_no_items .
    CONSTANTS:
      BEGIN OF c_segment_error,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_segment_error .
    CONSTANTS:
      BEGIN OF c_no_qualifier,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_no_qualifier .
    CONSTANTS:
      BEGIN OF c_no_g_number,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_no_g_number .
    CONSTANTS:
      BEGIN OF c_no_h_number,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_no_h_number .
    CONSTANTS:
      BEGIN OF c_missing_header,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_missing_header .
    CONSTANTS:
      BEGIN OF c_no_c_number,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_no_c_number .
    CONSTANTS:
      BEGIN OF c_no_e_number,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_no_e_number .
    CONSTANTS:
      BEGIN OF c_wrong_function,
        msgid TYPE symsgid VALUE 'ZSD_DEV790',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_wrong_function .
    CONSTANTS:
      BEGIN OF c_no_sold_to_party,
        msgid TYPE symsgid VALUE 'ZFI_DEV925',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_no_sold_to_party .
    DATA mf_segnum TYPE idocdsgnum .

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !mf_segnum TYPE idocdsgnum OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_invoice_mrm_example IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->mf_segnum = mf_segnum .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
