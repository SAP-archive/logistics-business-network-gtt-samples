interface ZIF_GTT_POF_EF_PROCESSOR
  public .


  methods CHECK_APP_OBJECTS
    raising
      CX_UDM_MESSAGE .
  methods CHECK_RELEVANCE
    returning
      value(RV_RESULT) type zif_gtt_pof_ef_types=>TV_CONDITION
    raising
      CX_UDM_MESSAGE .
  methods GET_CONTROL_DATA
    changing
      !CT_CONTROL_DATA type zif_gtt_pof_ef_types=>TT_CONTROL_DATA
    raising
      CX_UDM_MESSAGE .
  methods GET_TRACK_ID_DATA
    exporting
      !ET_TRACK_ID_DATA type zif_gtt_pof_ef_types=>TT_TRACK_ID_DATA
    raising
      CX_UDM_MESSAGE .
  methods GET_PLANNED_EVENTS
    changing
      !CT_EXPEVENTDATA type zif_gtt_pof_ef_types=>TT_EXPEVENTDATA
      !CT_MEASRMNTDATA type zif_gtt_pof_ef_types=>TT_MEASRMNTDATA
      !CT_INFODATA type zif_gtt_pof_ef_types=>TT_INFODATA
    raising
      CX_UDM_MESSAGE .
endinterface.
