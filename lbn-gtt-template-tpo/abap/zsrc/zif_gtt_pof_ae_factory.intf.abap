interface ZIF_GTT_POF_AE_FACTORY
  public .


  methods GET_AE_FILLER
    importing
      !IO_AE_PARAMETERS type ref to zif_gtt_pof_ae_parameters
    returning
      value(RO_AE_FILLER) type ref to zif_gtt_pof_ae_filler
    raising
      CX_UDM_MESSAGE .
  methods GET_AE_PARAMETERS
    importing
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_EVENT_TYPE type /SAPTRX/EVTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_EVENT_TYPE_CNTL_TABS type TRXAS_EVENTTYPE_TABS optional
      !IT_EVENTS type TRXAS_EVT_CTABS
    returning
      value(RO_AE_PARAMETERS) type ref to zif_gtt_pof_ae_parameters
    raising
      CX_UDM_MESSAGE .
  methods GET_AE_PROCESSOR
    importing
      !IS_DEFINITION type zif_gtt_pof_ef_types=>TS_DEFINITION
      !IO_AE_FACTORY type ref to ZIF_GTT_POF_AE_FACTORY
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_EVENT_TYPE type /SAPTRX/EVTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_EVENT_TYPE_CNTL_TABS type TRXAS_EVENTTYPE_TABS optional
      !IT_EVENTS type TRXAS_EVT_CTABS
    returning
      value(RO_AE_PROCESSOR) type ref to zif_gtt_pof_ae_processor
    raising
      CX_UDM_MESSAGE .
endinterface.
