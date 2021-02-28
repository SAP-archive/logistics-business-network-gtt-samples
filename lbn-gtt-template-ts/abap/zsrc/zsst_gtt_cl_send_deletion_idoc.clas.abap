class ZSST_GTT_CL_SEND_DELETION_IDOC definition
  public
  final
  create private .

public section.

  class-methods GET_INSTANCE
    importing
      !IT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT
    returning
      value(RO_UPDATER) type ref to ZSST_GTT_CL_SEND_DELETION_IDOC
    raising
      CX_UDM_MESSAGE .
  methods PREPARE_IDOC_DATA
    importing
      !IT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT
    raising
      CX_UDM_MESSAGE .
  methods SEND_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ts_aotype,
             tor_type    TYPE /scmtms/tor_type,
             aot_type    TYPE /saptrx/aotype,
             server_name TYPE /saptrx/trxservername,
           END OF ts_aotype .
  types:
    tt_aotype TYPE STANDARD TABLE OF ts_aotype WITH EMPTY KEY .
  types:
    tt_trxas_appobj_ctab TYPE STANDARD TABLE OF trxas_appobj_ctab_wa
                                  WITH EMPTY KEY .
  types:
    BEGIN OF ts_idoc_data,
             trxserv      TYPE /saptrx/trxserv,
             appsys       TYPE logsys,
             appobj_dtabs TYPE tt_trxas_appobj_ctab,
           END OF ts_idoc_data .
  types:
    tt_idoc_data TYPE STANDARD TABLE OF ts_idoc_data
                          WITH EMPTY KEY .
  types:
    BEGIN OF ts_lips_ex,
             vbeln TYPE lips-vbeln,
             posnr TYPE lips-posnr,
           END OF ts_lips_ex .
  types:
    tt_lips_ex TYPE STANDARD TABLE OF ts_lips_ex
                        WITH EMPTY KEY .
  types:
    BEGIN OF ts_ekpo_ex,
             ebeln TYPE ekpo-ebeln,
             ebelp TYPE ekpo-ebelp,
             lips  TYPE tt_lips_ex,
           END OF ts_ekpo_ex .
  types:
    tt_ekpo_ex TYPE SORTED TABLE OF ts_ekpo_ex
                        WITH UNIQUE KEY ebeln ebelp .
  types:
    BEGIN OF ts_lips_upd,
             vbeln TYPE lipsvb-vbeln,
             posnr TYPE lipsvb-posnr,
             vgbel TYPE lipsvb-vgbel,
             vgpos TYPE lipsvb-vgpos,
             updkz TYPE lipsvb-updkz,
           END OF ts_lips_upd .
  types:
    tt_lips_upd TYPE STANDARD TABLE OF ts_lips_upd .
  types TS_LIKP type LIKPVB .
  types TS_LIPS type LIPSVB .

  constants CV_IDOC_TYPE_EHPOST01 type EDI_IDOCTP value 'AOPOST' ##NO_TEXT.
  constants CV_MSG_TYPE_AOPOST type EDI_MESTYP value 'AOPOST' ##NO_TEXT.
  constants CV_PARTNER_TYPE type EDI_RCVPRT value 'LS' ##NO_TEXT.
  constants CV_SEG_E1EHPAO type EDILSEGTYP value 'E1EHPAO' ##NO_TEXT.
  constants CV_SEG_E1EHPTID type EDILSEGTYP value 'E1EHPTID' ##NO_TEXT.
  constants CV_STRUCTURE_PKG type DEVCLASS value '/SAPTRX/SCEM_AI_R3' ##NO_TEXT.
  constants CV_AOTYPE_MASK type /SAPTRX/AOTYPE value 'ZGTT_SHP%' ##NO_TEXT.
  constants CV_MAX_END_DATE type /SAPTRX/TID_END_DATE_TSLOC value '099991231000000' ##NO_TEXT.
  constants:
    BEGIN OF cs_system_fields,
                 actual_bisiness_timezone TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_TIMEZONE',
                 actual_bisiness_datetime TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_DATETIME',
               END OF cs_system_fields .
  constants:
    BEGIN OF cs_obj_type,
                 tms_tor TYPE /saptrx/trk_obj_type VALUE 'TMS_TOR',
               END OF cs_obj_type .
  constants:
    BEGIN OF cs_change_mode,
                 insert    TYPE updkz_d VALUE 'I',
                 update    TYPE updkz_d VALUE 'U',
                 delete    TYPE updkz_d VALUE 'D',
                 undefined TYPE updkz_d VALUE 'T',
               END OF cs_change_mode .
  data MV_APPSYS type LOGSYS .
  data MV_TZONE type TIMEZONE .
  data MT_AOTYPE type TT_AOTYPE .
  data MT_IDOC_DATA type TT_IDOC_DATA .

  methods SEND_IDOC_EHPOST01
    importing
      !IV_APPSYS type LOGSYS
      !IS_TRXSERV type /SAPTRX/TRXSERV
      !IT_APPOBJ_DTABS type TRXAS_APPOBJ_CTABS .
  methods FILL_IDOC_APPOBJ_DTABS
    importing
      !IS_AOTYPE type TS_AOTYPE
      !IS_TOR_ROOT type /SCMTMS/S_EM_BO_TOR_ROOT
    changing
      !CS_IDOC_DATA type TS_IDOC_DATA .
  methods FILL_IDOC_TRXSERV
    importing
      !IS_AOTYPE type TS_AOTYPE
    changing
      !CS_IDOC_DATA type TS_IDOC_DATA .
  methods INITIATE
    importing
      !IT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT
    raising
      CX_UDM_MESSAGE .
  class-methods IS_GTT_ENABLED
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods IS_EXTRACTOR_EXIST
    importing
      !IV_TRK_OBJ_TYPE type CLIKE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods THROW_EXCEPTION
    importing
      !IV_TEXTID type SOTR_CONC default ''
    raising
      CX_UDM_MESSAGE .
ENDCLASS.



CLASS ZSST_GTT_CL_SEND_DELETION_IDOC IMPLEMENTATION.


  METHOD fill_idoc_appobj_dtabs.
    cs_idoc_data-appobj_dtabs = VALUE #( BASE cs_idoc_data-appobj_dtabs (
      trxservername    = cs_idoc_data-trxserv-trx_server_id
      appobjtype       = is_aotype-aot_type
      appobjid         = is_tor_root-tor_id
      update_indicator = cs_change_mode-delete ) ).
  ENDMETHOD.


  METHOD fill_idoc_trxserv.
    SELECT SINGLE /saptrx/trxserv~trx_server_id
                  /saptrx/trxserv~trx_server
      INTO ( cs_idoc_data-trxserv-trx_server_id,
             cs_idoc_data-trxserv-trx_server )
      FROM /saptrx/trxserv
      WHERE trx_server_id = is_aotype-server_name.
  ENDMETHOD.


  METHOD get_instance.
    IF is_gtt_enabled( ) = abap_true.
      ro_updater  = NEW #( ).
      ro_updater->initiate( it_tor_root ).
    ELSE.
      MESSAGE e006(zsst_gtt) INTO DATA(lv_dummy).
      throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD initiate.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = mv_appsys
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.
    IF sy-subrc <> 0.
      MESSAGE e007(zsst_gtt) INTO DATA(lv_dummy).
      throw_exception( ).
    ENDIF.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = mv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      MESSAGE e003(zsst_gtt) INTO lv_dummy.
      throw_exception( ).
    ENDIF.

    SELECT /scmtms/c_torty~type          AS tor_type,
           /scmtms/c_torty~aotype        AS aot_type,
           /saptrx/aotypes~trxservername AS server_name
      INTO TABLE @mt_aotype
      FROM /scmtms/c_torty
      JOIN /saptrx/aotypes ON /scmtms/c_torty~aotype = /saptrx/aotypes~aotype
      FOR ALL ENTRIES IN @it_tor_root
      WHERE /scmtms/c_torty~type         = @it_tor_root-tor_type AND
            /saptrx/aotypes~trk_obj_type = @cs_obj_type-tms_tor  AND
            /saptrx/aotypes~torelevant   = @abap_true.
    IF sy-subrc <> 0.
      MESSAGE e008(zsst_gtt) INTO lv_dummy.
      throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD IS_EXTRACTOR_EXIST.
    DATA: lv_count  TYPE i VALUE 0.

    SELECT COUNT(*)
      INTO lv_count
      FROM /saptrx/aotypes
      WHERE trk_obj_type = iv_trk_obj_type
        AND torelevant   = abap_true.

    rv_result   = boolc( lv_count > 0 ).
  ENDMETHOD.


  METHOD is_gtt_enabled.

    DATA lv_extflag TYPE flag.

    rv_result = abap_false.

    " Check package dependent BADI disabling
    CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'
      EXPORTING
        i_structure_package = cv_structure_pkg
      IMPORTING
        e_active            = lv_extflag
      EXCEPTIONS
        not_existing        = 1
        object_not_existing = 2
        no_extension_object = 3
        OTHERS              = 4.

    IF sy-subrc = 0 AND lv_extflag = abap_true.
      " Check if any tracking server defined
      CALL FUNCTION '/SAPTRX/EVENT_MGR_CHECK'
        EXCEPTIONS
          no_event_mgr_available = 1
          OTHERS                 = 2.

      "Check whether at least 1 active extractor exists for every object
      IF sy-subrc = 0.
        rv_result = is_extractor_exist( iv_trk_obj_type = cs_obj_type-tms_tor ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD prepare_idoc_data.

    DATA ls_idoc_data TYPE ts_idoc_data.


    LOOP AT it_tor_root ASSIGNING FIELD-SYMBOL(<ls_tor_root>).
      CLEAR: ls_idoc_data.
      ls_idoc_data-appsys = mv_appsys.

      ASSIGN mt_aotype[ tor_type = <ls_tor_root>-tor_type ] TO FIELD-SYMBOL(<ls_aotype>).

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      fill_idoc_appobj_dtabs(
        EXPORTING
          is_aotype    = <ls_aotype>
          is_tor_root  = <ls_tor_root>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      IF ls_idoc_data-appobj_dtabs[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD send_idoc_data.
    LOOP AT mt_idoc_data ASSIGNING FIELD-SYMBOL(<ls_idoc_data>).
      send_idoc_ehpost01(
          iv_appsys       = <ls_idoc_data>-appsys
          is_trxserv      = <ls_idoc_data>-trxserv
          it_appobj_dtabs = <ls_idoc_data>-appobj_dtabs ).
    ENDLOOP.
  ENDMETHOD.


  METHOD send_idoc_ehpost01.

    DATA:
      ls_e1ehpao                    TYPE e1ehpao,
      ls_e1ehptid                   TYPE e1ehptid,
      ls_master_idoc_control        TYPE edidc,
      lt_master_idoc_data           TYPE edidd_tt,
      ls_master_idoc_data           TYPE edidd,
      lt_communication_idoc_control TYPE edidc_tt,
      lr_appobj_dtabs               TYPE REF TO trxas_appobj_ctab_wa.

    ls_master_idoc_control-rcvprt = cv_partner_type.
    ls_master_idoc_control-rcvprn = is_trxserv-trx_server.
    ls_master_idoc_control-mestyp = cv_msg_type_aopost.
    ls_master_idoc_control-idoctp = cv_idoc_type_ehpost01.

    LOOP AT it_appobj_dtabs REFERENCE INTO lr_appobj_dtabs WHERE trxservername = is_trxserv-trx_server_id.
      CLEAR ls_e1ehpao.

      ls_master_idoc_data-mandt   = sy-mandt.
      ls_master_idoc_data-segnam  = cv_seg_e1ehpao.
      ls_e1ehpao-appsys           = iv_appsys.
      ls_e1ehpao-appobjtype       = lr_appobj_dtabs->appobjtype.
      ls_e1ehpao-appobjid         = lr_appobj_dtabs->appobjid.
      ls_e1ehpao-update_indicator = lr_appobj_dtabs->update_indicator.
      ls_master_idoc_data-sdata   = ls_e1ehpao.
      INSERT ls_master_idoc_data INTO TABLE lt_master_idoc_data.

      " Dummy entry for TID is needed, since this is mandatory segment in the IDOC
      CLEAR ls_e1ehptid.
      ls_master_idoc_data-segnam = cv_seg_e1ehptid.
      ls_master_idoc_data-sdata  = ls_e1ehptid.
      INSERT ls_master_idoc_data INTO TABLE lt_master_idoc_data.

    ENDLOOP.

    IF lt_master_idoc_data IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
      EXPORTING
        master_idoc_control        = ls_master_idoc_control
      TABLES
        communication_idoc_control = lt_communication_idoc_control
        master_idoc_data           = lt_master_idoc_data.

  ENDMETHOD.


  METHOD THROW_EXCEPTION.
    RAISE EXCEPTION TYPE cx_udm_message
      EXPORTING
        textid  = iv_textid
        m_msgid = sy-msgid
        m_msgty = sy-msgty
        m_msgno = sy-msgno
        m_msgv1 = sy-msgv1
        m_msgv2 = sy-msgv2
        m_msgv3 = sy-msgv3
        m_msgv4 = sy-msgv4.
  ENDMETHOD.
ENDCLASS.
