CLASS zcl_gtt_pof_event_rel_dl_main DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS constructor
    IMPORTING
      !io_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters
      !is_app_objects TYPE trxas_appobj_ctab_wa OPTIONAL .
  METHODS initiate
    RAISING
      cx_udm_message .
  METHODS is_enabled
    IMPORTING
      !iv_milestone TYPE clike
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  METHODS update .
PROTECTED SECTION.

  DATA mo_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters .
  DATA ms_app_objects TYPE trxas_appobj_ctab_wa .
  DATA ms_relevance TYPE zpof_gtt_ee_rel .

  METHODS get_field_name
  ABSTRACT
    IMPORTING
      !iv_milestone TYPE clike
      !iv_internal TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rv_field_name) TYPE fieldname
    RAISING
      cx_udm_message .
  METHODS get_object_status
  ABSTRACT
    IMPORTING
      !iv_milestone TYPE clike
    RETURNING
      VALUE(rv_value) TYPE /saptrx/paramval200
    RAISING
      cx_udm_message .
PRIVATE SECTION.

  METHODS set_relevance
    IMPORTING
      !iv_milestone TYPE clike
      !iv_relevance TYPE clike
    RAISING
      cx_udm_message .
  METHODS recalc_relevance
    IMPORTING
      !iv_milestone TYPE clike
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS ZCL_GTT_POF_EVENT_REL_DL_MAIN IMPLEMENTATION.


  METHOD constructor.

    mo_ef_parameters  = io_ef_parameters.
    ms_app_objects    = is_app_objects.

  ENDMETHOD.


  METHOD initiate.

    DATA(lv_appobjid) = ms_app_objects-appobjid.

    " read stored statuses
    SELECT SINGLE *
      INTO @ms_relevance
      FROM zpof_gtt_ee_rel
      WHERE appobjid  = @lv_appobjid.

    " initiate statuses with initial values
    IF sy-subrc <> 0.
      CLEAR: ms_relevance.
      ms_relevance-appobjid = lv_appobjid.

      " recalculate statuses
      recalc_relevance( iv_milestone = zif_gtt_pof_app_constants=>cs_milestone-dl_put_away ).
      recalc_relevance( iv_milestone = zif_gtt_pof_app_constants=>cs_milestone-dl_packing ).
      recalc_relevance( iv_milestone = zif_gtt_pof_app_constants=>cs_milestone-dl_goods_receipt ).
      recalc_relevance( iv_milestone = zif_gtt_pof_app_constants=>cs_milestone-dl_pod ).
    ENDIF.

  ENDMETHOD.


  METHOD is_enabled.

    DATA(lv_field_name) = get_field_name(
                            iv_milestone   = iv_milestone
                            iv_internal = abap_true ).

    ASSIGN COMPONENT lv_field_name OF STRUCTURE ms_relevance
      TO FIELD-SYMBOL(<lv_value>).

    IF <lv_value> IS ASSIGNED.
      rv_result   = <lv_value>.
    ELSE.
      MESSAGE e001(zgtt_pof) WITH lv_field_name '' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD recalc_relevance.

    " retrieve delivery item status field value
    DATA(lv_status)     = get_object_status( iv_milestone = iv_milestone ).

    " calculate relevance
    "   for shipment POD planning :
    "     status is <> 'empty' -> relevant
    "   for other delivery item level planned events:
    "     initial value is 'A' -> relevant
    DATA(lv_relevance)  = COND abap_bool(
      WHEN lv_status = zif_gtt_pof_app_constants=>cs_delivery_stat-not_relevant
        THEN abap_false
      WHEN lv_status = zif_gtt_pof_app_constants=>cs_delivery_stat-not_processed OR
           iv_milestone = zif_gtt_pof_app_constants=>cs_milestone-dl_pod
        THEN abap_true
        ELSE abap_undefined
    ).

    " update flag value if it has appropriate value
    IF lv_relevance <> abap_undefined.
      set_relevance(
        EXPORTING
          iv_milestone = iv_milestone
          iv_relevance = lv_relevance ).
    ENDIF.

  ENDMETHOD.


  METHOD set_relevance.

    DATA(lv_fname_int) = get_field_name(
                           iv_milestone = iv_milestone
                           iv_internal  = abap_true ).

    ASSIGN COMPONENT lv_fname_int OF STRUCTURE ms_relevance
      TO FIELD-SYMBOL(<lv_flag>).

    IF <lv_flag> IS ASSIGNED.
      <lv_flag>   = iv_relevance.
    ELSE.
      MESSAGE e001(zgtt_pof) WITH lv_fname_int '' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD update.


    CALL FUNCTION 'ZPOF_GTT_UPDATE_RELEVANCE_TAB'
      IN UPDATE TASK
      EXPORTING
        is_relevance = ms_relevance.


  ENDMETHOD.
ENDCLASS.
