FUNCTION zgtt_add_trackid_ote_shphdr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_APPSYS) TYPE  /SAPTRX/APPLSYSTEM
*"     REFERENCE(I_APP_OBJ_TYPES) TYPE  /SAPTRX/AOTYPES
*"     REFERENCE(I_ALL_APPL_TABLES) TYPE  TRXAS_TABCONTAINER
*"     REFERENCE(I_APP_TYPE_CNTL_TABS) TYPE  TRXAS_APPTYPE_TABS
*"     REFERENCE(I_APP_OBJECTS) TYPE  TRXAS_APPOBJ_CTABS
*"  TABLES
*"      E_TRACKIDDATA STRUCTURE  /SAPTRX/TRACK_ID_DATA
*"      E_LOGTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      TID_DETERMINATION_ERROR
*"      TABLE_DETERMINATION_ERROR
*"      STOP_PROCESSING
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Top Include
* TYPE-POOLS:trxas.
*----------------------------------------------------------------------*

*****<< THIS IS JUST EXAMPLE CODING >>*****
  DATA:
*   Definition of all application objects
    ls_app_objects TYPE trxas_appobj_ctab_wa,
    lv_tzone       TYPE timezone,
    lt_yvttk       TYPE STANDARD TABLE OF vttkvb,
    ls_yvttk       TYPE vttkvb.

  FIELD-SYMBOLS:
*   Work Structure for Shipment
    <ls_xvttk> TYPE vttkvb.

* Read Header Data old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_header_old
    CHANGING lt_yvttk.

  CLEAR e_trackiddata.

  e_trackiddata-appsys     = i_appsys.
  e_trackiddata-appobjtype = i_app_obj_types-aotype.

* <3> Loop at application objects for geting Shipment data
  LOOP AT i_app_objects INTO ls_app_objects.

*   Application Object ID
    e_trackiddata-appobjid   = ls_app_objects-appobjid.

*   Check if Main table is Shipment or not.
    IF ls_app_objects-maintabdef <> gc_bpt_shipment_header_new.
      PERFORM create_logtable_aot
        TABLES e_logtable
        USING  ls_app_objects-maintabdef
               space
               i_app_obj_types-controldatafunc
               ls_app_objects-appobjtype
               i_appsys.
      RAISE cdata_determination_error.
    ELSE.
*     Read Main Object Table
      ASSIGN ls_app_objects-maintabref->* TO <ls_xvttk>.
    ENDIF.

*   Actual Business Time zone
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.

    e_trackiddata-trxcod = 'SHIPMENT_ORDER'.
    e_trackiddata-trxid = <ls_xvttk>-tknum.
    CONCATENATE '0' sy-datum sy-uzeit INTO e_trackiddata-start_date.
    e_trackiddata-end_date = '099991231000000'.
    e_trackiddata-timzon = lv_tzone.
    e_trackiddata-msrid = space.
    APPEND e_trackiddata.

    e_trackiddata-trxcod = 'RESOURCE'.
    CLEAR e_trackiddata-start_date.
    CLEAR e_trackiddata-end_date.
    CLEAR e_trackiddata-timzon.

    IF <ls_xvttk>-updkz = 'I'.
      IF <ls_xvttk>-vsart = '01' AND <ls_xvttk>-exti1 IS NOT INITIAL.
        CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-exti1 INTO e_trackiddata-trxid.
        e_trackiddata-msrid = space.
        APPEND e_trackiddata.
      ELSEIF <ls_xvttk>-vsart = '04' AND <ls_xvttk>-signi IS NOT INITIAL.
        CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-signi INTO e_trackiddata-trxid.
        e_trackiddata-msrid = space.
        APPEND e_trackiddata.
      ENDIF.
    ELSEIF <ls_xvttk>-updkz = 'U'.
      READ TABLE lt_yvttk INTO ls_yvttk INDEX 1.
      CHECK ls_yvttk IS NOT INITIAL.

      IF <ls_xvttk>-vsart <> ls_yvttk-vsart.
        IF <ls_xvttk>-vsart = '01' AND <ls_xvttk>-exti1 IS NOT INITIAL.
          CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-exti1 INTO e_trackiddata-trxid.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ELSEIF <ls_xvttk>-vsart = '04' AND <ls_xvttk>-signi IS NOT INITIAL.
          CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-signi INTO e_trackiddata-trxid.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ENDIF.

        IF ls_yvttk-vsart = '01' AND ls_yvttk-exti1 IS NOT INITIAL.
          CONCATENATE ls_yvttk-tknum ls_yvttk-exti1 INTO e_trackiddata-trxid.
          e_trackiddata-action = 'D'.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ELSEIF ls_yvttk-vsart = '04' AND ls_yvttk-signi IS NOT INITIAL.
          CONCATENATE ls_yvttk-tknum ls_yvttk-signi INTO e_trackiddata-trxid.
          e_trackiddata-action = 'D'.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ENDIF.
      ELSEIF <ls_xvttk>-vsart = '01' AND <ls_xvttk>-exti1 <> ls_yvttk-exti1.
        IF ls_yvttk-exti1 IS INITIAL.
          CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-exti1 INTO e_trackiddata-trxid.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ELSEIF <ls_xvttk>-exti1 IS INITIAL.
          CONCATENATE ls_yvttk-tknum ls_yvttk-exti1 INTO e_trackiddata-trxid.
          e_trackiddata-action = 'D'.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ELSE.
          CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-exti1 INTO e_trackiddata-trxid.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.

          CONCATENATE ls_yvttk-tknum ls_yvttk-exti1 INTO e_trackiddata-trxid.
          e_trackiddata-action = 'D'.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ENDIF.
      ELSEIF <ls_xvttk>-vsart = '04' AND <ls_xvttk>-signi <> ls_yvttk-signi.
        IF ls_yvttk-signi IS INITIAL.
          CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-signi INTO e_trackiddata-trxid.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ELSEIF <ls_xvttk>-signi IS INITIAL.
          CONCATENATE ls_yvttk-tknum ls_yvttk-signi INTO e_trackiddata-trxid.
          e_trackiddata-action = 'D'.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ELSE.
          CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-signi INTO e_trackiddata-trxid.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.

          CONCATENATE ls_yvttk-tknum ls_yvttk-signi INTO e_trackiddata-trxid.
          e_trackiddata-action = 'D'.
          e_trackiddata-msrid = space.
          APPEND e_trackiddata.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
