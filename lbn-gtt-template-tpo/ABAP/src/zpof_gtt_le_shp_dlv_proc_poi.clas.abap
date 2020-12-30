CLASS zpof_gtt_le_shp_dlv_proc_poi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_badi_interface .
    INTERFACES if_ex_le_shp_delivery_proc .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpof_gtt_le_shp_dlv_proc_poi IMPLEMENTATION.
  METHOD if_ex_le_shp_delivery_proc~change_fcode_attributes.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~change_delivery_item.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~check_item_deletion.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~change_field_attributes.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~delivery_final_check.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~delivery_deletion.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~fill_delivery_header.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~document_number_publish.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~change_delivery_header.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~initialize_delivery.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~fill_delivery_item.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~save_document_prepare.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~save_and_publish_document.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~publish_delivery_item.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~item_deletion.
  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~save_and_publish_before_output.
    CALL FUNCTION 'ZPOF_GTT_CTP_DL_TO_PO'
      EXPORTING
        it_likp_new = it_xlikp
        it_lips_new = it_xlips
        it_lips_old = it_ylips.

*    TRY.
*        DATA(lo_updater)  = zpof_gtt_dlv_updater_for_poi=>get_instance( ).
*
*        lo_updater->prepare_idoc_data(
*          EXPORTING
*            it_likp_new = it_xlikp
*            it_lips_new = it_xlips
*            it_lips_old = it_ylips ).
*
*        lo_updater->send_idoc_data( ).  "TBD: save bapiret into log
*
*      CATCH cx_udm_message INTO DATA(lo_udm_message).
*        "TBD: save into log
*    ENDTRY.

  ENDMETHOD.

  METHOD if_ex_le_shp_delivery_proc~read_delivery.
  ENDMETHOD.

ENDCLASS.
