CLASS zsst_gtt_cl_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_delivery_item,
        delivery_number      TYPE vbeln_vl,
        delivery_item_number TYPE posnr_vl,
        freight_unit_number  TYPE /scmtms/tor_id,
        change_mode          TYPE /bobf/conf_change_mode,
        stop_id_dst          TYPE /scmtms/stop_id,
        log_locid_dst        TYPE /scmtms/location_id,
        country_code_dst     TYPE land1,
        city_name_dst	       TYPE	ad_city1,
        evt_exp_datetime     TYPE /saptrx/event_exp_datetime,
        evt_exp_tzone        TYPE /saptrx/timezone,
      END OF ty_delivery_item .
    TYPES:
      tt_delivery_item TYPE TABLE OF ty_delivery_item .
    CLASS-DATA gt_delivery_item TYPE tt_delivery_item .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF ts_likp,
      vbeln TYPE likp-vbeln,
      vstel TYPE likp-vstel,
      lfart TYPE likp-lfart,
      vsbed TYPE likp-vsbed,
    END OF ts_likp .
  TYPES:
    BEGIN OF ts_vbak,
      vbeln TYPE vbak-vbeln,
      vkorg TYPE vbak-vkorg,
      vtweg TYPE vbak-vtweg,
      spart TYPE vbak-spart,
      auart TYPE vbak-auart,
      vsbed TYPE vbak-vsbed,
    END OF ts_vbak .
  TYPES:
    BEGIN OF ts_vbfa,
      vbelv   TYPE vbfa-vbelv,
      posnv   TYPE vbfa-posnv,
      vbeln   TYPE vbfa-vbeln,
      posnn   TYPE vbfa-posnn,
      vbtyp_n TYPE vbfa-vbtyp_n,
    END OF ts_vbfa .
  TYPES:
    BEGIN OF ts_tms_c_shp,
      vstel       TYPE tms_c_shp-vstel,
      lfart       TYPE tms_c_shp-lfart,
      vsbed       TYPE tms_c_shp-vsbed,
      tm_ctrl_key TYPE tms_c_shp-tm_ctrl_key,
    END OF ts_tms_c_shp .
  TYPES:
    BEGIN OF ts_tms_c_sls,
      vkorg       TYPE tms_c_sls-vkorg,
      vtweg       TYPE tms_c_sls-vtweg,
      spart       TYPE tms_c_sls-spart,
      auart       TYPE tms_c_sls-auart,
      vsbed       TYPE tms_c_sls-vsbed,
      tm_ctrl_key TYPE tms_c_sls-tm_ctrl_key,
    END OF ts_tms_c_sls .
  TYPES:
    BEGIN OF ts_tms_c_control,
      tm_ctrl_key   TYPE tms_c_control-tm_ctrl_key,
      sls_to_tm_ind TYPE tms_c_control-sls_to_tm_ind,
      od_to_tm_ind  TYPE tms_c_control-od_to_tm_ind,
    END OF ts_tms_c_control .
ENDCLASS.



CLASS ZSST_GTT_CL_TOOLKIT IMPLEMENTATION.
ENDCLASS.
