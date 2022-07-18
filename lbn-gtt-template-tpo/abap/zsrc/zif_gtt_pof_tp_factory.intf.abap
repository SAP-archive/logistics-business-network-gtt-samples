interface ZIF_GTT_POF_TP_FACTORY
  public .


  methods GET_BO_READER
    importing
      !IO_EF_PARAMETERS type ref to zif_gtt_pof_ef_parameters
    returning
      value(RO_BO_READER) type ref to ZIF_GTT_POF_TP_READER .
  methods GET_EF_PARAMETERS
    importing
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_APP_OBJ_TYPES type /SAPTRX/AOTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_APP_TYPE_CNTL_TABS type TRXAS_APPTYPE_TABS optional
      !IT_APP_OBJECTS type TRXAS_APPOBJ_CTABS
    returning
      value(RO_EF_PARAMETERS) type ref to zif_gtt_pof_ef_parameters .
  methods GET_EF_PROCESSOR
    importing
      !IS_DEFINITION type zif_gtt_pof_ef_types=>TS_DEFINITION
      !IO_BO_FACTORY type ref to ZIF_GTT_POF_TP_FACTORY
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_APP_OBJ_TYPES type /SAPTRX/AOTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_APP_TYPE_CNTL_TABS type TRXAS_APPTYPE_TABS
      !IT_APP_OBJECTS type TRXAS_APPOBJ_CTABS
    returning
      value(RO_EF_PROCESSOR) type ref to zif_gtt_pof_ef_processor
    raising
      CX_UDM_MESSAGE .
  methods GET_PE_FILLER
    importing
      !IO_EF_PARAMETERS type ref to zif_gtt_pof_ef_parameters
      !IO_BO_READER type ref to ZIF_GTT_POF_TP_READER
    returning
      value(RO_PE_FILLER) type ref to zif_gtt_pof_pe_filler
    raising
      CX_UDM_MESSAGE .
endinterface.
