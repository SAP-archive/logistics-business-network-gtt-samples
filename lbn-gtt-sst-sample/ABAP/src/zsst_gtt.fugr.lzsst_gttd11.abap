*&---------------------------------------------------------------------*
*& Local class definition - General for Actual Events
*&---------------------------------------------------------------------*

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ae_performer DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS check_relevance
      IMPORTING
        is_definition           TYPE lif_ef_types=>ts_definition
        io_ae_factory           TYPE REF TO lif_ae_factory
        iv_appsys               TYPE /saptrx/applsystem
        is_event_type           TYPE /saptrx/evtypes
        it_all_appl_tables      TYPE trxas_tabcontainer
        it_event_type_cntl_tabs TYPE trxas_eventtype_tabs OPTIONAL
        is_events               TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_result)        TYPE lif_ef_types=>tv_condition
      RAISING
        cx_udm_message.

    CLASS-METHODS get_event_data
      IMPORTING
        is_definition           TYPE lif_ef_types=>ts_definition
        io_ae_factory           TYPE REF TO lif_ae_factory
        iv_appsys               TYPE /saptrx/applsystem
        is_event_type           TYPE /saptrx/evtypes
        it_all_appl_tables      TYPE trxas_tabcontainer
        it_event_type_cntl_tabs TYPE trxas_eventtype_tabs
        it_events               TYPE trxas_evt_ctabs
      CHANGING
        ct_eventid_map          TYPE trxas_evtid_evtcnt_map
        ct_trackingheader       TYPE lif_ae_types=>tt_trackingheader
        ct_tracklocation        TYPE lif_ae_types=>tt_tracklocation
        ct_trackreferences      TYPE lif_ae_types=>tt_trackreferences
        ct_trackparameters      TYPE lif_ae_types=>tt_trackparameters
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_ae_performer IMPLEMENTATION.
  METHOD check_relevance.
    DATA: lt_events TYPE trxas_evt_ctabs.

    lt_events = VALUE #( ( is_events ) ).

    DATA(lo_ae_processor) = io_ae_factory->get_ae_processor(
                              is_definition           = is_definition
                              io_ae_factory           = io_ae_factory
                              iv_appsys               = iv_appsys
                              is_event_type           = is_event_type
                              it_all_appl_tables      = it_all_appl_tables
                              it_event_type_cntl_tabs = it_event_type_cntl_tabs
                              it_events               = lt_events ).

    lo_ae_processor->check_events( ).

    rv_result = lo_ae_processor->check_relevance( ).
  ENDMETHOD.

  METHOD get_event_data.
    DATA(lo_ae_processor) = io_ae_factory->get_ae_processor(
                              is_definition           = is_definition
                              io_ae_factory           = io_ae_factory
                              iv_appsys               = iv_appsys
                              is_event_type           = is_event_type
                              it_all_appl_tables      = it_all_appl_tables
                              it_event_type_cntl_tabs = it_event_type_cntl_tabs
                              it_events               = it_events ).

    lo_ae_processor->check_events( ).

    lo_ae_processor->get_event_data(
      CHANGING
        ct_eventid_map     = ct_eventid_map
        ct_trackingheader  = ct_trackingheader
        ct_tracklocation   = ct_tracklocation
        ct_trackreferences = ct_trackreferences
        ct_trackparameters = ct_trackparameters ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_ae_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ae_processor.

    METHODS constructor
      IMPORTING
        is_definition    TYPE lif_ef_types=>ts_definition
        io_ae_parameters TYPE REF TO lif_ae_parameters
        io_ae_filler     TYPE REF TO lif_ae_filler.

  PRIVATE SECTION.
    DATA: ms_definition    TYPE lif_ef_types=>ts_definition,
          mo_ae_parameters TYPE REF TO lif_ae_parameters,
          mo_ae_filler     TYPE REF TO lif_ae_filler.
ENDCLASS.

CLASS lcl_ae_processor IMPLEMENTATION.
  METHOD constructor.
    ms_definition    = is_definition.
    mo_ae_parameters = io_ae_parameters.
    mo_ae_filler     = io_ae_filler.
  ENDMETHOD.

  METHOD lif_ae_processor~check_events.
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

        lcl_tools=>throw_exception(
          iv_textid = lif_ef_constants=>cs_errors-table_determination ).

      ELSEIF ms_definition-mastertab IS NOT INITIAL AND
            <ls_events>-mastertabdef <> ms_definition-mastertab.
        MESSAGE e088(/saptrx/asc)
          WITH <ls_events>-mastertabdef
               mo_ae_parameters->get_event_type( )-eventdatafunc
               <ls_events>-eventtype
               mo_ae_parameters->get_appsys( )
          INTO lv_dummy.

        lcl_tools=>throw_exception(
          iv_textid = lif_ef_constants=>cs_errors-table_determination ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_ae_processor~check_relevance.
    DATA(lr_events)  = mo_ae_parameters->get_events( ).

    FIELD-SYMBOLS <lt_events>  TYPE trxas_evt_ctabs.

    ASSIGN lr_events->* TO <lt_events>.

    rv_result = lif_ef_constants=>cs_condition-false.

    LOOP AT <lt_events> ASSIGNING FIELD-SYMBOL(<ls_events>).
      rv_result   = mo_ae_filler->check_relevance( is_events = <ls_events> ).

      IF rv_result = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_ae_processor~get_event_data.
    DATA: lt_eventid_map     TYPE trxas_evtid_evtcnt_map,
          lt_trackingheader  TYPE lif_ae_types=>tt_trackingheader,
          lt_tracklocation   TYPE lif_ae_types=>tt_tracklocation,
          lt_trackreferences TYPE lif_ae_types=>tt_trackreferences,
          lt_trackparameters TYPE lif_ae_types=>tt_trackparameters.

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
          ct_trackparameters = lt_trackparameters ).
    ENDLOOP.

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

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ae_parameters DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_ae_parameters.

    METHODS constructor
      IMPORTING
        iv_appsys               TYPE /saptrx/applsystem
        is_event_type           TYPE /saptrx/evtypes
        it_all_appl_tables      TYPE trxas_tabcontainer
        it_event_type_cntl_tabs TYPE trxas_eventtype_tabs
        it_events               TYPE trxas_evt_ctabs.

  PRIVATE SECTION.
    DATA: mv_appsys               TYPE /saptrx/applsystem,
          ms_event_type           TYPE /saptrx/evtypes,
          mr_all_appl_tables      TYPE REF TO data,         "trxas_tabcontainer
          mr_event_type_cntl_tabs TYPE REF TO data,         "trxas_eventtype_tabs
          mr_events               TYPE REF TO data.         "trxas_evt_ctabs

ENDCLASS.

CLASS lcl_ae_parameters IMPLEMENTATION.
  METHOD constructor.
    mv_appsys               = iv_appsys.
    ms_event_type           = is_event_type.
    mr_all_appl_tables      = REF #( it_all_appl_tables ).
    mr_event_type_cntl_tabs = REF #( it_event_type_cntl_tabs ).
    mr_events               = REF #( it_events ).
  ENDMETHOD.

  METHOD lif_ae_parameters~get_appl_table.
    TRY.
        FIELD-SYMBOLS: <lt_all_appl_tables> TYPE trxas_tabcontainer.

        ASSIGN mr_all_appl_tables->* TO <lt_all_appl_tables>.

        rr_data   = <lt_all_appl_tables>[ tabledef = iv_tabledef ]-tableref.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE e008(/saptrx/asc)
          WITH iv_tabledef
               ms_event_type-eventdatafunc
          INTO DATA(lv_dummy).

        lcl_tools=>throw_exception(
          iv_textid = lif_ef_constants=>cs_errors-stop_processing ).
    ENDTRY.
  ENDMETHOD.

  METHOD lif_ae_parameters~get_appsys.
    rv_appsys   = mv_appsys.
  ENDMETHOD.

  METHOD lif_ae_parameters~get_events.
    rr_data     = mr_events.
  ENDMETHOD.

  METHOD lif_ae_parameters~get_event_type.
    rs_event_type   = ms_event_type.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ae_factory DEFINITION
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES: lif_ae_factory
      ABSTRACT METHODS get_ae_filler.

ENDCLASS.

CLASS lcl_ae_factory IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_parameters.
    ro_ae_parameters  = NEW lcl_ae_parameters(
      iv_appsys               = iv_appsys
      is_event_type           = is_event_type
      it_all_appl_tables      = it_all_appl_tables
      it_event_type_cntl_tabs = it_event_type_cntl_tabs
      it_events               = it_events ).
  ENDMETHOD.

  METHOD lif_ae_factory~get_ae_processor.
    DATA: lo_ae_parameters TYPE REF TO lif_ae_parameters,
          lo_ae_filler     TYPE REF TO lif_ae_filler.

    lo_ae_parameters  = lif_ae_factory~get_ae_parameters(
                          iv_appsys               = iv_appsys
                          is_event_type           = is_event_type
                          it_all_appl_tables      = it_all_appl_tables
                          it_event_type_cntl_tabs = it_event_type_cntl_tabs
                          it_events               = it_events ).

    lo_ae_filler      = lif_ae_factory~get_ae_filler(
                          io_ae_parameters = lo_ae_parameters ).

    ro_ae_processor   = NEW lcl_ae_processor(
                          is_definition    = is_definition
                          io_ae_parameters = lo_ae_parameters
                          io_ae_filler     = lo_ae_filler ).

  ENDMETHOD.
ENDCLASS.
