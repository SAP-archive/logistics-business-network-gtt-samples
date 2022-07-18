interface ZIF_GTT_POF_TP_READER
  public .


  methods CHECK_RELEVANCE
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RV_RESULT) type zif_gtt_pof_ef_types=>TV_CONDITION
    raising
      CX_UDM_MESSAGE .
  methods GET_DATA
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RR_DATA) type ref to DATA
    raising
      CX_UDM_MESSAGE .
  methods GET_DATA_OLD
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RR_DATA) type ref to DATA
    raising
      CX_UDM_MESSAGE .
  methods GET_FIELD_PARAMETER
    importing
      !IV_FIELD_NAME type CLIKE
      !IV_PARAMETER type zif_gtt_pof_ef_types=>TV_PARAMETER_ID
    returning
      value(RV_RESULT) type zif_gtt_pof_ef_types=>TV_PARAMETER_VALUE
    raising
      CX_UDM_MESSAGE .
  methods GET_MAPPING_STRUCTURE
    returning
      value(RR_DATA) type ref to DATA .
  methods GET_TRACK_ID_DATA
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
    exporting
      !ET_TRACK_ID_DATA type zif_gtt_pof_ef_types=>TT_TRACK_ID_DATA
    raising
      CX_UDM_MESSAGE .
endinterface.
