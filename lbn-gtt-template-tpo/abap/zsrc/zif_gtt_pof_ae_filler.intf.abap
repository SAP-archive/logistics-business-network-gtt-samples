interface ZIF_GTT_POF_AE_FILLER
  public .


  methods CHECK_RELEVANCE
    importing
      !IS_EVENTS type TRXAS_EVT_CTAB_WA
    returning
      value(RV_RESULT) type zif_gtt_pof_ef_types=>TV_CONDITION
    raising
      CX_UDM_MESSAGE .
  methods GET_EVENT_DATA
    importing
      !IS_EVENTS type TRXAS_EVT_CTAB_WA
    changing
      !CT_EVENTID_MAP type TRXAS_EVTID_EVTCNT_MAP
      !CT_TRACKINGHEADER type zif_gtt_pof_ae_types=>TT_TRACKINGHEADER
      !CT_TRACKLOCATION type zif_gtt_pof_ae_types=>TT_TRACKLOCATION
      !CT_TRACKREFERENCES type zif_gtt_pof_ae_types=>TT_TRACKREFERENCES
      !CT_TRACKPARAMETERS type zif_gtt_pof_ae_types=>TT_TRACKPARAMETERS
    raising
      CX_UDM_MESSAGE .
endinterface.
