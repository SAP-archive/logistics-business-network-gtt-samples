CLASS zcl_gtt_pof_ae_processor DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_ae_processor .

  METHODS constructor
    IMPORTING
      !is_definition TYPE zif_gtt_pof_ef_types=>ts_definition
      !io_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters
      !io_ae_filler TYPE REF TO zif_gtt_pof_ae_filler .
PROTECTED SECTION.
PRIVATE SECTION.

  DATA ms_definition TYPE zif_gtt_pof_ef_types=>ts_definition .
  DATA mo_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters .
  DATA mo_ae_filler TYPE REF TO zif_gtt_pof_ae_filler .

  METHODS add_technical_records
    IMPORTING
      !it_eventid_map TYPE trxas_evtid_evtcnt_map
    CHANGING
      !ct_trackparameters TYPE zif_gtt_pof_ae_types=>tt_trackparameters
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_ae_processor IMPLEMENTATION.


  METHOD add_technical_records.

    DATA: lt_eventid    TYPE STANDARD TABLE OF /saptrx/evtcnt.

    lt_eventid  = VALUE #( FOR ls_eventid_map IN it_eventid_map
                           ( ls_eventid_map-evtcnt ) ).

    SORT lt_eventid.
    DELETE ADJACENT DUPLICATES FROM lt_eventid.

    LOOP AT lt_eventid ASSIGNING FIELD-SYMBOL(<lv_eventid>).
      ct_trackparameters    = VALUE #( BASE ct_trackparameters
        (
          evtcnt      = <lv_eventid>
          param_name  = zif_gtt_pof_ef_constants=>cs_system_fields-actual_technical_timezone
          param_value = zcl_gtt_pof_tools=>get_system_time_zone( )
        )
        (
          evtcnt      = <lv_eventid>
          param_name  = zif_gtt_pof_ef_constants=>cs_system_fields-actual_technical_datetime
          param_value = zcl_gtt_pof_tools=>get_system_date_time( )
        ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    ms_definition    = is_definition.
    mo_ae_parameters = io_ae_parameters.
    mo_ae_filler     = io_ae_filler.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_processor~check_events.

    DATA: lr_events TYPE REF TO data,
          lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <lt_events>  TYPE trxas_evt_ctabs.

    lr_events  = mo_ae_parameters->get_events( ).

    ASSIGN lr_events->* TO <lt_events>.

    LOOP AT <lt_events> ASSIGNING FIELD-SYMBOL(<ls_events>).
      IF <ls_events>-maintabdef <> ms_definition-maintab.
        MESSAGE e087(/saptrx/asc)
          WITH <ls_events>-maintabdef
               mo_ae_parameters->get_event_type( )-eventdatafunc
               <ls_events>-eventtype
               mo_ae_parameters->get_appsys( )
          INTO lv_dummy.

        zcl_gtt_pof_tools=>throw_exception(
          iv_textid = zif_gtt_pof_ef_constants=>cs_errors-table_determination ).

      ELSEIF ms_definition-mastertab IS NOT INITIAL AND
            <ls_events>-mastertabdef <> ms_definition-mastertab.
        MESSAGE e088(/saptrx/asc)
          WITH <ls_events>-mastertabdef
               mo_ae_parameters->get_event_type( )-eventdatafunc
               <ls_events>-eventtype
               mo_ae_parameters->get_appsys( )
          INTO lv_dummy.

        zcl_gtt_pof_tools=>throw_exception(
          iv_textid = zif_gtt_pof_ef_constants=>cs_errors-table_determination ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_processor~check_relevance.

    DATA(lr_events)  = mo_ae_parameters->get_events( ).

    FIELD-SYMBOLS <lt_events>  TYPE trxas_evt_ctabs.

    ASSIGN lr_events->* TO <lt_events>.

    rv_result = zif_gtt_pof_ef_constants=>cs_condition-false.

    LOOP AT <lt_events> ASSIGNING FIELD-SYMBOL(<ls_events>).
      rv_result   = mo_ae_filler->check_relevance( is_events = <ls_events> ).

      IF rv_result = zif_gtt_pof_ef_constants=>cs_condition-true.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_processor~get_event_data.

    DATA: lt_eventid_map     TYPE trxas_evtid_evtcnt_map,
          lt_trackingheader  TYPE zif_gtt_pof_ae_types=>tt_trackingheader,
          lt_tracklocation   TYPE zif_gtt_pof_ae_types=>tt_tracklocation,
          lt_trackreferences TYPE zif_gtt_pof_ae_types=>tt_trackreferences,
          lt_trackparameters TYPE zif_gtt_pof_ae_types=>tt_trackparameters.

    DATA(lr_events)    = mo_ae_parameters->get_events( ).

    FIELD-SYMBOLS <lt_events>  TYPE trxas_evt_ctabs.

    ASSIGN lr_events->* TO <lt_events>.

    LOOP AT <lt_events> ASSIGNING FIELD-SYMBOL(<ls_events>)
      WHERE maintabdef = ms_definition-maintab.

      mo_ae_filler->get_event_data(
        EXPORTING
          is_events          = <ls_events>
        CHANGING
          ct_eventid_map     = lt_eventid_map
          ct_trackingheader  = lt_trackingheader
          ct_tracklocation   = lt_tracklocation
          ct_trackreferences = lt_trackreferences
          ct_trackparameters = lt_trackparameters
      ).
    ENDLOOP.

    add_technical_records(
      EXPORTING
        it_eventid_map     = lt_eventid_map
      CHANGING
        ct_trackparameters = lt_trackparameters ).

    " Add all the changes to result tables in the end of the method,
    " so that in case of exceptions there will be no inconsistent data in them
    ct_eventid_map[]      = VALUE #( BASE ct_eventid_map
                                     ( LINES OF lt_eventid_map ) ).
    ct_trackingheader[]   = VALUE #( BASE ct_trackingheader
                                     ( LINES OF lt_trackingheader ) ).
    ct_tracklocation[]    = VALUE #( BASE ct_tracklocation
                                     ( LINES OF lt_tracklocation ) ).
    ct_trackreferences[]  = VALUE #( BASE ct_trackreferences
                                     ( LINES OF lt_trackreferences ) ).
    ct_trackparameters[]  = VALUE #( BASE ct_trackparameters
                                     ( LINES OF lt_trackparameters ) ).

  ENDMETHOD.
ENDCLASS.
