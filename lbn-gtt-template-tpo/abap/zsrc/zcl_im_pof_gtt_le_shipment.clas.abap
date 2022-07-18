CLASS zcl_im_pof_gtt_le_shipment DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_badi_le_shipment .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IM_POF_GTT_LE_SHIPMENT IMPLEMENTATION.


  METHOD if_ex_badi_le_shipment~at_save.
  ENDMETHOD.


  METHOD if_ex_badi_le_shipment~before_update.
    CALL FUNCTION 'ZPOF_GTT_CTP_SH_TO_DL'
      EXPORTING
        is_shipment = im_shipments_before_update.
  ENDMETHOD.


  METHOD if_ex_badi_le_shipment~in_update.
  ENDMETHOD.
ENDCLASS.
