CLASS zcl_gtt_pof_ctp_snd DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS send_idoc_data
    EXPORTING
      !et_bapiret TYPE bapiret2_t
    RAISING
      cx_udm_message .
PROTECTED SECTION.

  TYPES:
    BEGIN OF ts_aotype,
             obj_type    TYPE /saptrx/trk_obj_type,
             aot_type    TYPE /saptrx/aotype,
             server_name TYPE /saptrx/trxservername,
           END OF ts_aotype .
  TYPES:
    tt_aotype TYPE STANDARD TABLE OF ts_aotype WITH EMPTY KEY .
  TYPES:
    tt_trxas_appobj_ctab TYPE STANDARD TABLE OF trxas_appobj_ctab_wa
                                  WITH EMPTY KEY .
  TYPES:
    tt_aotype_rst  TYPE RANGE OF /saptrx/aotype .
  TYPES:
    tt_trk_obj_type TYPE STANDARD TABLE OF /saptrx/trk_obj_type
                             WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_idoc_data,
             control      TYPE /saptrx/bapi_trk_control_tab,
             info         TYPE /saptrx/bapi_trk_info_tab,
             tracking_id  TYPE /saptrx/bapi_trk_trkid_tab,
             exp_event    TYPE /saptrx/bapi_trk_ee_tab,
             trxserv      TYPE /saptrx/trxserv,
             appsys       TYPE logsys,
             appobj_ctabs TYPE tt_trxas_appobj_ctab,
           END OF ts_idoc_data .
  TYPES:
    tt_idoc_data TYPE STANDARD TABLE OF ts_idoc_data
                          WITH EMPTY KEY .

  DATA mv_appsys TYPE logsys .
  DATA mt_aotype TYPE tt_aotype .
  DATA mt_idoc_data TYPE tt_idoc_data .

  METHODS get_aotype_restrictions
  ABSTRACT
    EXPORTING
      !et_aotype TYPE tt_aotype_rst .
  METHODS get_object_type
  ABSTRACT
    RETURNING
      VALUE(rv_objtype) TYPE /saptrx/trk_obj_type .
  METHODS fill_idoc_trxserv
    IMPORTING
      !is_aotype TYPE ts_aotype
    CHANGING
      !cs_idoc_data TYPE ts_idoc_data .
  METHODS initiate
    RAISING
      cx_udm_message .
  METHODS initiate_aotypes
    RAISING
      cx_udm_message .
  CLASS-METHODS is_extractor_exist
    IMPORTING
      !iv_trk_obj_type TYPE clike
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
  CLASS-METHODS is_gtt_enabled
    IMPORTING
      !it_trk_obj_type TYPE tt_trk_obj_type
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_ctp_snd IMPLEMENTATION.


  METHOD fill_idoc_trxserv.

    SELECT SINGLE *
      INTO cs_idoc_data-trxserv
      FROM /saptrx/trxserv
      WHERE trx_server_id = is_aotype-server_name.

  ENDMETHOD.


  METHOD initiate.

    " Get current logical system
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = mv_appsys
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

    IF sy-subrc <> 0.
      MESSAGE e007(zgtt_pof) INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

    initiate_aotypes( ).


  ENDMETHOD.


  METHOD initiate_aotypes.

    DATA: lt_aotype_rst TYPE tt_aotype_rst.

    DATA(lv_objtype)  = get_object_type(  ).

    get_aotype_restrictions(
      IMPORTING
        et_aotype = lt_aotype_rst ).

    " Prepare AOT list
    SELECT trk_obj_type  AS obj_type
           aotype        AS aot_type
           trxservername AS server_name
      INTO TABLE mt_aotype
      FROM /saptrx/aotypes
      WHERE trk_obj_type  = lv_objtype
        AND aotype       IN lt_aotype_rst
        AND torelevant    = abap_true.

    IF sy-subrc <> 0.
      MESSAGE e008(zgtt_pof) INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_extractor_exist.

    DATA: lv_trk_obj_type  TYPE /saptrx/aotypes-trk_obj_type.

    SELECT SINGLE trk_obj_type
      INTO lv_trk_obj_type
      FROM /saptrx/aotypes
      WHERE trk_obj_type = iv_trk_obj_type
        AND torelevant   = abap_true.

    rv_result   = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_gtt_enabled.

    DATA: lv_extflag      TYPE flag.

    rv_result   = abap_false.

    " Check package dependent BADI disabling
    CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'
      EXPORTING
        i_structure_package = zif_gtt_pof_ef_constants=>cv_structure_pkg
      IMPORTING
        e_active            = lv_extflag
      EXCEPTIONS
        not_existing        = 1
        object_not_existing = 2
        no_extension_object = 3
        OTHERS              = 4.

    IF sy-subrc = 0 AND lv_extflag = abap_true.
*     Check if any tracking server defined
      CALL FUNCTION '/SAPTRX/EVENT_MGR_CHECK'
        EXCEPTIONS
          no_event_mgr_available = 1
          OTHERS                 = 2.

      "Check whether at least 1 active extractor exists for every object
      IF sy-subrc = 0.
        rv_result = boolc( it_trk_obj_type[] IS NOT INITIAL ).

        LOOP AT it_trk_obj_type ASSIGNING FIELD-SYMBOL(<lv_trk_obj_type>).
          IF is_extractor_exist( iv_trk_obj_type = <lv_trk_obj_type> ) = abap_false.
            rv_result   = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD send_idoc_data.

    DATA: lt_bapiret1 TYPE bapiret2_t,
          lt_bapiret2 TYPE bapiret2_t.

    LOOP AT mt_idoc_data ASSIGNING FIELD-SYMBOL(<ls_idoc_data>).
      CLEAR: lt_bapiret1[], lt_bapiret2[].

      /saptrx/cl_send_idocs=>send_idoc_ehpost01(
        EXPORTING
          it_control      = <ls_idoc_data>-control
          it_info         = <ls_idoc_data>-info
          it_tracking_id  = <ls_idoc_data>-tracking_id
          it_exp_event    = <ls_idoc_data>-exp_event
          is_trxserv      = <ls_idoc_data>-trxserv
          iv_appsys       = <ls_idoc_data>-appsys
          it_appobj_ctabs = <ls_idoc_data>-appobj_ctabs
          iv_upd_task     = 'X'
        IMPORTING
          et_bapireturn   = lt_bapiret1 ).

      " when GTT.2 version
      IF /saptrx/cl_send_idocs=>st_idoc_data[] IS NOT INITIAL.
        /saptrx/cl_send_idocs=>send_idoc_gttmsg01(
          IMPORTING
            et_bapireturn = lt_bapiret2 ).
      ENDIF.

      " collect messages, if it is necessary
      IF et_bapiret IS REQUESTED.
        et_bapiret    = VALUE #( BASE et_bapiret
                                 ( LINES OF lt_bapiret1 )
                                 ( LINES OF lt_bapiret2 ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
