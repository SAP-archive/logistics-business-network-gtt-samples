interface ZIF_GTT_POF_PE_FILLER
  public .


  methods CHECK_RELEVANCE
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RV_RESULT) type zif_gtt_pof_ef_types=>TV_CONDITION
    raising
      CX_UDM_MESSAGE .
  methods GET_PLANED_EVENTS
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_EXPEVENTDATA type zif_gtt_pof_ef_types=>TT_EXPEVENTDATA
      !CT_MEASRMNTDATA type zif_gtt_pof_ef_types=>TT_MEASRMNTDATA
      !CT_INFODATA type zif_gtt_pof_ef_types=>TT_INFODATA
    raising
      CX_UDM_MESSAGE .
endinterface.
