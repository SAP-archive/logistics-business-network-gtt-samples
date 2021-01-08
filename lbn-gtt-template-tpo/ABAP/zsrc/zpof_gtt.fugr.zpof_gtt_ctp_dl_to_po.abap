FUNCTION zpof_gtt_ctp_dl_to_po.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_LIKP_NEW) TYPE  SHP_LIKP_T
*"     REFERENCE(IT_LIPS_NEW) TYPE  SHP_LIPS_T
*"     REFERENCE(IT_LIPS_OLD) TYPE  SHP_LIPS_T
*"----------------------------------------------------------------------
  TRY.
      DATA(lo_item) = lcl_ctp_sender_dl_to_po_item=>get_instance( ).

      " Delivery header
      lo_item->prepare_idoc_data(
        EXPORTING
          it_likp_new = it_likp_new
          it_lips_new = it_lips_new
          it_lips_old = it_lips_old ).

      lo_item->send_idoc_data( ). "TBD: store messages into log

    CATCH cx_udm_message.
  ENDTRY.
ENDFUNCTION.
