FUNCTION zgtt_add_trackid_ote_deitem.
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
    lv_tzone       TYPE timezone.

  FIELD-SYMBOLS:
*   Work Structure for Delivery Item
    <ls_xlips> TYPE lipsvb.


  CLEAR e_trackiddata.

  e_trackiddata-appsys     = i_appsys.
  e_trackiddata-appobjtype = i_app_obj_types-aotype.

* <3> Loop at application objects for geting delivery item data
  LOOP AT i_app_objects INTO ls_app_objects.

*   Application Object ID
    e_trackiddata-appobjid   = ls_app_objects-appobjid.

*   Check if Main table is Delivery Item or not.
    IF ls_app_objects-maintabdef <> gc_bpt_delivery_item_new.
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
      ASSIGN ls_app_objects-maintabref->* TO <ls_xlips>.
    ENDIF.

*   Actual Business Time zone
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.

    e_trackiddata-trxcod = 'OUTBOUND_DELIVERY_IT'.
    CONCATENATE <ls_xlips>-vbeln <ls_xlips>-posnr INTO e_trackiddata-trxid.
    CONCATENATE '0' sy-datum sy-uzeit INTO e_trackiddata-start_date.
    e_trackiddata-end_date = '099991231000000'.
    e_trackiddata-timzon = lv_tzone.
    e_trackiddata-msrid = space.
    APPEND e_trackiddata.

    IF <ls_xlips>-updkz = 'I'.
      e_trackiddata-trxcod = 'OUTBOUND_DELIVERY'.
      e_trackiddata-trxid = <ls_xlips>-vbeln.
      CONCATENATE '0' sy-datum sy-uzeit INTO e_trackiddata-start_date.
      e_trackiddata-end_date = '099991231000000'.
      e_trackiddata-timzon = lv_tzone.
      e_trackiddata-msrid = space.
      APPEND e_trackiddata.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
