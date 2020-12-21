FUNCTION zpof_gtt_ctp_sh_to_dl.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_SHIPMENT) TYPE  CXSHIPMENT
*"----------------------------------------------------------------------
  TRY.
      DATA(lo_head) = lcl_ctp_sender_sh_to_dl_head=>get_instance( ).
      DATA(lo_ship) = NEW lcl_ctp_shipment_data( is_shipment = is_shipment ).

      " Delivery header
      lo_head->prepare_idoc_data( io_ship_data = lo_ship ).

      lo_head->send_idoc_data( ). "TBD: store messages into log

      " Delivery item
      DATA(lo_item) = lcl_ctp_sender_sh_to_dl_item=>get_instance( ).

      lo_item->prepare_idoc_data( io_ship_data  = lo_ship ).

      lo_item->send_idoc_data(  ).

    CATCH cx_udm_message.
  ENDTRY.

ENDFUNCTION.
