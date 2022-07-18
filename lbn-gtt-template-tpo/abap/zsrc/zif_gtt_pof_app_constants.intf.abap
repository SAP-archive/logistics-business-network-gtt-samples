interface ZIF_GTT_POF_APP_CONSTANTS
  public .


  constants:
    BEGIN OF cs_tabledef,
               po_header_new       TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ORDER_HEADER_NEW',
               po_header_old       TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ORDER_HEADER_OLD',
               po_item_new         TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ITEM_NEW',
               po_item_old         TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ITEM_OLD',
               po_sched_new        TYPE /saptrx/strucdatadef VALUE 'PO_SCHED_LINE_ITEM_NEW',
               po_sched_old        TYPE /saptrx/strucdatadef VALUE 'PO_SCHED_LINE_ITEM_OLD',
               po_vend_conf_new    TYPE /saptrx/strucdatadef VALUE 'VENDOR_CONFIRMATION_NEW',
               po_vend_conf_old    TYPE /saptrx/strucdatadef VALUE 'VENDOR_CONFIRMATION_OLD',
               md_material_header  TYPE /saptrx/strucdatadef VALUE 'MATERIAL_HEADER',
               md_material_segment TYPE /saptrx/strucdatadef VALUE 'MATERIAL_SEGMENT',
               md_update_control   TYPE /saptrx/strucdatadef VALUE 'UPDATE_CONTROL',
               dl_header_new       TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER_NEW',
               dl_header_old       TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER_OLD',
               dl_hdr_status_new   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HDR_STATUS_NEW',
               dl_hdr_status_old   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HDR_STATUS_OLD',
               dl_item_new         TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_NEW',
               dl_item_old         TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_OLD',
               dl_partners_new     TYPE /saptrx/strucdatadef VALUE 'PARTNERS_NEW',
               dl_partners_old     TYPE /saptrx/strucdatadef VALUE 'PARTNERS_OLD',
               dl_itm_status_new   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_STATUS_NEW',
               dl_itm_status_old   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_STATUS_OLD',
               dl_hu_item_new      TYPE /saptrx/strucdatadef VALUE 'HU_ITEM_NEW',
               dl_hu_item_old      TYPE /saptrx/strucdatadef VALUE 'HU_ITEM_OLD',
               sh_header_new       TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_HEADER_NEW',
               sh_header_old       TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_HEADER_OLD',
               sh_item_new         TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_NEW',
               sh_item_old         TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_OLD',
               sh_stage_new        TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_LEG_NEW',
               sh_stage_old        TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_LEG_OLD',
               sh_item_stage_new   TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_LEG_NEW',
               sh_item_stage_old   TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_LEG_OLD',
               sh_delivery_header  TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER',
               sh_delivery_item    TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM',
             END OF cs_tabledef .
  constants:
    BEGIN OF cs_system_fields,
               actual_bisiness_timezone TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_TIMEZONE',
               actual_bisiness_datetime TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_DATETIME',
             END OF cs_system_fields .
  constants:
    BEGIN OF cs_trxcod,
               po_number   TYPE /saptrx/trxcod VALUE 'PURCHASE_ORDER',
               po_position TYPE /saptrx/trxcod VALUE 'PURCHASE_ORDER_ITEM',
               dl_number   TYPE /saptrx/trxcod VALUE 'INBOUND_DELIVERY',
               dl_position TYPE /saptrx/trxcod VALUE 'INBOUND_DELIVERY_IT',
               sh_number   TYPE /saptrx/trxcod VALUE 'INBOUND_SHIPMENT',
               sh_resource TYPE /saptrx/trxcod VALUE 'INBOUND_RESOURCE',
             END OF cs_trxcod .
  constants:
    BEGIN OF cs_relevance,
               bsart TYPE ekko-bsart VALUE 'NB',    "publish to github as 'NB'
               ebtyp TYPE ekes-ebtyp VALUE 'AB',
               lfart TYPE likp-lfart VALUE 'EL',
               pstyv TYPE lips-pstyv VALUE 'ELN',
               shtyp TYPE vttk-shtyp VALUE '0010',
             END OF cs_relevance .
  constants:
    BEGIN OF cs_milestone,
               po_confirmation  TYPE /saptrx/appl_event_tag VALUE 'CONFIRMATION',
               po_goods_receipt TYPE /saptrx/appl_event_tag VALUE 'GOODS_RECEIPT',
               po_undeletion    TYPE /saptrx/appl_event_tag VALUE 'UNDELETION',
               po_deletion      TYPE /saptrx/appl_event_tag VALUE 'DELETION',
               dl_put_away      TYPE /saptrx/appl_event_tag VALUE 'PUT_AWAY',
               dl_packing       TYPE /saptrx/appl_event_tag VALUE 'PACKING',
               dl_goods_receipt TYPE /saptrx/appl_event_tag VALUE 'GOODS_RECEIPT',
               dl_pod           TYPE /saptrx/appl_event_tag VALUE 'SHP_POD',
               sh_check_in      TYPE /saptrx/appl_event_tag VALUE 'CHECK_IN',
               sh_load_start    TYPE /saptrx/appl_event_tag VALUE 'LOAD_BEGIN',
               sh_load_end      TYPE /saptrx/appl_event_tag VALUE 'LOAD_END',
               sh_departure     TYPE /saptrx/appl_event_tag VALUE 'DEPARTURE',
               sh_arrival       TYPE /saptrx/appl_event_tag VALUE 'ARRIV_DEST',
               sh_pod           TYPE /saptrx/appl_event_tag VALUE 'POD',
             END OF cs_milestone .
  constants:
    BEGIN OF cs_event_param,
               quantity      TYPE /saptrx/paramname VALUE 'QUANTITY',
               confirm_type  TYPE /saptrx/paramname VALUE 'CONFIRM_TYPE',
               reversal      TYPE /saptrx/paramname VALUE 'REVERSAL_INDICATOR',
               location_id   TYPE /saptrx/paramname VALUE 'LOCATION_ID',
               location_type TYPE /saptrx/paramname VALUE 'LOCATION_TYPE',
             END OF cs_event_param .
  constants:
    BEGIN OF cs_bstae,
               confirm  TYPE ekpo-bstae VALUE '0001',
               delivery TYPE ekpo-bstae VALUE '0004',
             END OF cs_bstae .
  constants:
    BEGIN OF cs_loekz,
               active  TYPE ekpo-loekz VALUE '',
               deleted TYPE ekpo-loekz VALUE 'L',
             END OF cs_loekz .
  constants:
    BEGIN OF cs_delivery_stat,
               not_relevant    TYPE wbsta VALUE '',
               not_processed   TYPE wbsta VALUE 'A',
               partially_proc  TYPE wbsta VALUE 'B',
               completely_proc TYPE wbsta VALUE 'C',
             END OF cs_delivery_stat .
  constants:
    BEGIN OF cs_md_type,
               goods_receipt TYPE mkpf-blart VALUE 'WE',
             END OF cs_md_type .
  constants:
    BEGIN OF cs_parvw,
               supplier TYPE parvw VALUE 'LF',
             END OF cs_parvw .
  constants:
    BEGIN OF cs_adrtype,
               organization TYPE ad_adrtype VALUE '1',
             END OF cs_adrtype .
  constants:
    BEGIN OF cs_loccat,
               departure TYPE zif_gtt_pof_app_types=>tv_loccat VALUE 'S',
               arrival   TYPE zif_gtt_pof_app_types=>tv_loccat VALUE 'D',
             END OF cs_loccat .
  constants:
    BEGIN OF cs_vbtyp,
               shipment TYPE vbtyp VALUE '8',
               delivery TYPE vbtyp VALUE '7',
             END OF cs_vbtyp .
  constants:
    BEGIN OF cs_start_evtcnt,
               shipment TYPE i VALUE 1000000000,
               delivery TYPE i VALUE 1100000000,
             END OF cs_start_evtcnt .
  constants CV_AGENT_ID_TYPE type BU_ID_TYPE value 'LBN001' ##NO_TEXT.
  constants:
    cv_agent_id_prefix TYPE c LENGTH 4 value 'LBN#' ##NO_TEXT.
endinterface.
