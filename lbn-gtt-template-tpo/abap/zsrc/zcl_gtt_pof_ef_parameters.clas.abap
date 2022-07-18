CLASS zcl_gtt_pof_ef_parameters DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_ef_parameters .

  METHODS constructor
    IMPORTING
      !iv_appsys TYPE /saptrx/applsystem
      !is_app_obj_types TYPE /saptrx/aotypes
      !it_all_appl_tables TYPE trxas_tabcontainer
      !it_app_type_cntl_tabs TYPE trxas_apptype_tabs OPTIONAL
      !it_app_objects TYPE trxas_appobj_ctabs .
PROTECTED SECTION.
PRIVATE SECTION.

  DATA mv_appsys TYPE /saptrx/applsystem .
  DATA ms_app_obj_types TYPE /saptrx/aotypes .
  DATA mr_all_appl_tables TYPE REF TO data .
  DATA mr_app_type_cntl_tabs TYPE REF TO data .
  DATA mr_app_objects TYPE REF TO data .
ENDCLASS.



CLASS zcl_gtt_pof_ef_parameters IMPLEMENTATION.


  METHOD constructor.

    mv_appsys               = iv_appsys.
    ms_app_obj_types        = is_app_obj_types.
    mr_all_appl_tables      = REF #( it_all_appl_tables ).
    IF it_app_type_cntl_tabs IS SUPPLIED.
      mr_app_type_cntl_tabs = REF #( it_app_type_cntl_tabs ).
    ENDIF.
    mr_app_objects          = REF #( it_app_objects ).

  ENDMETHOD.


  METHOD zif_gtt_pof_ef_parameters~get_appl_table.

    TRY.
        FIELD-SYMBOLS: <lt_all_appl_tables>   TYPE trxas_tabcontainer.

        ASSIGN mr_all_appl_tables->* TO <lt_all_appl_tables>.

        rr_data   = <lt_all_appl_tables>[ tabledef = iv_tabledef ]-tableref.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE e008(/saptrx/asc)
          WITH iv_tabledef
               ms_app_obj_types-aotype
          INTO DATA(lv_dummy).

        zcl_gtt_pof_tools=>throw_exception(
          iv_textid = zif_gtt_pof_ef_constants=>cs_errors-stop_processing ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_gtt_pof_ef_parameters~get_appsys.

    rv_appsys           = mv_appsys.

  ENDMETHOD.


  METHOD zif_gtt_pof_ef_parameters~get_app_objects.

    rr_data             = mr_app_objects.

  ENDMETHOD.


  METHOD zif_gtt_pof_ef_parameters~get_app_obj_types.

    rs_app_obj_types    = ms_app_obj_types.

  ENDMETHOD.
ENDCLASS.
