*----------------------------------------------------------------------*
***INCLUDE LZGTT_SOFF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*       Create Log table for Tr-cd: /SAPTRX/ASAPLOG
*----------------------------------------------------------------------*
*      <-> CT_LOGTABLE       Logging table
*      --> IV_MAINTABDEF     Main table
*      --> IV_MASTERTABDEF   Master table
*      --> IV_EXTRACTOR      Extractor
*      --> IV_APPOBJTYPE     Application Object type
*      --> IV_APPSYS         Application system
*----------------------------------------------------------------------*
FORM create_logtable_ao_rel
  TABLES   ct_logtable     STRUCTURE bapiret2
  USING    iv_maintabdef   TYPE /saptrx/strucdatadef
           iv_mastertabdef TYPE /saptrx/strucdatadef
           iv_extractor    TYPE /saptrx/trrelfunc
           iv_appobjtype   TYPE /saptrx/aotype
           iv_appsys       TYPE /saptrx/applsystem.

  DATA:
    ls_bapiret LIKE bapiret2.

  IF NOT iv_maintabdef IS INITIAL.
    MESSAGE ID gc_msg_class TYPE gc_msg_type_e NUMBER gc_msg_no_087
      WITH iv_maintabdef
           iv_extractor
           gc_msg_val_aot
           iv_appobjtype
      INTO ls_bapiret-message.
    ls_bapiret-type       = gc_msg_type_e.
    ls_bapiret-id         = gc_msg_class.
    ls_bapiret-number     = gc_msg_no_087.
    ls_bapiret-message_v1 = iv_maintabdef.
    ls_bapiret-message_v2 = iv_extractor.
    ls_bapiret-message_v3 = gc_msg_val_aot.
    ls_bapiret-message_v4 = iv_appobjtype.
    ls_bapiret-system     = iv_appsys.
    APPEND ls_bapiret TO ct_logtable.
  ENDIF.

  IF NOT iv_mastertabdef IS INITIAL.
    MESSAGE ID gc_msg_class TYPE gc_msg_type_e NUMBER gc_msg_no_088
      WITH iv_mastertabdef
           iv_extractor
           gc_msg_val_aot
           iv_appobjtype
      INTO ls_bapiret-message.
    ls_bapiret-type       = gc_msg_type_e.
    ls_bapiret-id         = gc_msg_class.
    ls_bapiret-number     = gc_msg_no_088.
    ls_bapiret-message_v1 = iv_mastertabdef.
    ls_bapiret-message_v2 = iv_extractor.
    ls_bapiret-message_v3 = gc_msg_val_aot.
    ls_bapiret-message_v4 = iv_appobjtype.
    ls_bapiret-system     = iv_appsys.
    APPEND ls_bapiret TO ct_logtable.
  ENDIF.

ENDFORM.                    " create_logtable_ao_rel
*&---------------------------------------------------------------------*
*&      Form  create_logtable_et_rel
*&---------------------------------------------------------------------*
*       Create Log table for Tr-cd: /SAPTRX/ASAPLOG
*----------------------------------------------------------------------*
*      <-> CT_LOGTABLE       Logging table
*      --> IV_MAINTABDEF     Main table
*      --> IV_MASTERTABDEF   Master table
*      --> IV_EXTRACTOR      Extractor
*      --> IV_EVENTTYPE      Event type
*      --> IV_APPSYS         Application System
*----------------------------------------------------------------------*
FORM create_logtable_et_rel
  TABLES   ct_logtable     STRUCTURE bapiret2
  USING    iv_maintabdef   TYPE /saptrx/strucdatadef
           iv_mastertabdef TYPE /saptrx/strucdatadef
           iv_extractor    TYPE /saptrx/trrelfunc
           iv_eventtype    TYPE /saptrx/evtype
           iv_appsys       TYPE /saptrx/applsystem.

  DATA:
    ls_bapiret LIKE bapiret2.

  IF NOT iv_maintabdef IS INITIAL.
    MESSAGE ID gc_msg_class TYPE gc_msg_type_e NUMBER gc_msg_no_087
      WITH iv_maintabdef
           iv_extractor
           gc_msg_val_et
           iv_eventtype
      INTO ls_bapiret-message.
    ls_bapiret-type       = gc_msg_type_e.
    ls_bapiret-id         = gc_msg_class.
    ls_bapiret-number     = gc_msg_no_087.
    ls_bapiret-message_v1 = iv_maintabdef.
    ls_bapiret-message_v2 = iv_extractor.
    ls_bapiret-message_v3 = gc_msg_val_et.
    ls_bapiret-message_v4 = iv_eventtype.
    ls_bapiret-system     = iv_appsys.
    APPEND ls_bapiret TO ct_logtable.
  ENDIF.

  IF NOT iv_mastertabdef IS INITIAL.
    MESSAGE ID gc_msg_class TYPE gc_msg_type_e NUMBER gc_msg_no_088
      WITH iv_mastertabdef
           iv_extractor
           gc_msg_val_et
           iv_eventtype
      INTO ls_bapiret-message.
    ls_bapiret-type       = gc_msg_type_e.
    ls_bapiret-id         = gc_msg_class.
    ls_bapiret-number     = gc_msg_no_088.
    ls_bapiret-message_v1 = iv_mastertabdef.
    ls_bapiret-message_v2 = iv_extractor.
    ls_bapiret-message_v3 = gc_msg_val_et.
    ls_bapiret-message_v4 = iv_eventtype.
    ls_bapiret-system     = iv_appsys.
    APPEND ls_bapiret TO ct_logtable.
  ENDIF.

ENDFORM.                    " create_logtable_et_rel
*&---------------------------------------------------------------------*
*&      Form  create_logtable_aot
*&---------------------------------------------------------------------*
*       Create Log table for Tr-cd: /SAPTRX/ASAPLOG
*----------------------------------------------------------------------*
*      <-> CT_LOGTABLE       Logging table
*      --> IV_MAINTABDEF     Main table
*      --> IV_MASTERTABDEF   Master table
*      --> IV_EXTRACTOR      Extractor
*      --> IV_APPOBJTYPE     Application Object type
*      --> IV_APPSYS         Application system
*----------------------------------------------------------------------*
FORM create_logtable_aot
  TABLES ct_logtable     STRUCTURE bapiret2
  USING  iv_maintabdef   TYPE /saptrx/strucdatadef
         iv_mastertabdef TYPE /saptrx/strucdatadef
         iv_extractor    TYPE /saptrx/trinfodatafunc
         iv_appobjtype   TYPE /saptrx/aotype
         iv_appsys       TYPE /saptrx/applsystem.

  DATA: ls_bapiret LIKE bapiret2.

  IF NOT iv_maintabdef IS INITIAL.
    MESSAGE ID gc_msg_class TYPE gc_msg_type_e NUMBER gc_msg_no_087
      WITH iv_maintabdef
           iv_extractor
           gc_msg_val_aot
           iv_appobjtype
      INTO ls_bapiret-message.
    ls_bapiret-type       = gc_msg_type_e.
    ls_bapiret-id         = gc_msg_class.
    ls_bapiret-number     = gc_msg_no_087.
    ls_bapiret-message_v1 = iv_maintabdef.
    ls_bapiret-message_v2 = iv_extractor.
    ls_bapiret-message_v3 = gc_msg_val_aot.
    ls_bapiret-message_v4 = iv_appobjtype.
    ls_bapiret-system     = iv_appsys.
    APPEND ls_bapiret TO ct_logtable.
  ENDIF.

  IF NOT iv_mastertabdef IS INITIAL.
    MESSAGE ID gc_msg_class TYPE gc_msg_type_e NUMBER gc_msg_no_088
      WITH iv_mastertabdef
           iv_extractor
           gc_msg_val_aot
           iv_appobjtype
      INTO ls_bapiret-message.
    ls_bapiret-type       = gc_msg_type_e.
    ls_bapiret-id         = gc_msg_class.
    ls_bapiret-number     = gc_msg_no_088.
    ls_bapiret-message_v1 = iv_mastertabdef.
    ls_bapiret-message_v2 = iv_extractor.
    ls_bapiret-message_v3 = gc_msg_val_aot.
    ls_bapiret-message_v4 = iv_appobjtype.
    ls_bapiret-system     = iv_appsys.
    APPEND ls_bapiret TO ct_logtable.
  ENDIF.

ENDFORM.                    " create_logtable_aot
*&---------------------------------------------------------------------*
*&      Form  READ_APPL_TABLE
*&---------------------------------------------------------------------*
*      Used for generic reading of the application tables from the     *
*      EM data container
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables             *
*      --> I_TABLEDEF            Definition Name of the table          *
*      <-- CT_APPL_TABLE         Table content
*----------------------------------------------------------------------*
FORM read_appl_table
  USING    i_all_appl_tables TYPE trxas_tabcontainer
           i_tabledef        TYPE /saptrx/strucdatadef
  CHANGING ct_appl_table     TYPE ANY TABLE.

  DATA:
    ls_one_app_tables        TYPE trxas_tabcontainer_wa.

  FIELD-SYMBOLS:
    <lt_appl_table>          TYPE STANDARD TABLE.

* Get the Table Content                                              *
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
    WITH KEY tabledef = i_tabledef.
  IF NOT sy-subrc IS INITIAL.
    RAISE stop_processing.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_appl_table>.
    ct_appl_table[] = <lt_appl_table>[].
  ENDIF.

ENDFORM.                    " READ_APPL_TABLE
*&---------------------------------------------------------------------*
*&      Form  create_logtable_et
*&---------------------------------------------------------------------*
*       Create Log table for Tr-cd: /SAPTRX/ASAPLOG
*----------------------------------------------------------------------*
*      <-> CT_LOGTABLE       Logging table
*      --> IV_MAINTABDEF     Main table
*      --> IV_MASTERTABDEF   Master table
*      --> IV_EXTRACTOR      Extractor
*      --> IV_EVENTTYPE      Event type
*      --> IV_APPSYS         Application system
*----------------------------------------------------------------------*
FORM create_logtable_et
  TABLES   ct_logtable     STRUCTURE bapiret2
  USING    iv_maintabdef   TYPE /saptrx/strucdatadef
           iv_mastertabdef TYPE /saptrx/strucdatadef
           iv_extractor    TYPE /saptrx/treventdatafunc
           iv_eventtype    TYPE /saptrx/evtype
           iv_appsys       TYPE /saptrx/applsystem.

  DATA: ls_bapiret LIKE bapiret2.

  IF NOT iv_maintabdef IS INITIAL.
    MESSAGE ID gc_msg_class TYPE gc_msg_type_e NUMBER gc_msg_no_087
      WITH iv_maintabdef
           iv_extractor
           gc_msg_val_et
           iv_eventtype
      INTO ls_bapiret-message.
    ls_bapiret-type       = gc_msg_type_e.
    ls_bapiret-id         = gc_msg_class.
    ls_bapiret-number     = gc_msg_no_087.
    ls_bapiret-message_v1 = iv_maintabdef.
    ls_bapiret-message_v2 = iv_extractor.
    ls_bapiret-message_v3 = gc_msg_val_et.
    ls_bapiret-message_v4 = iv_eventtype.
    ls_bapiret-system     = iv_appsys.
    APPEND ls_bapiret TO ct_logtable.
  ENDIF.

  IF NOT iv_mastertabdef IS INITIAL.
    MESSAGE ID gc_msg_class TYPE gc_msg_type_e NUMBER gc_msg_no_088
      WITH iv_mastertabdef
           iv_extractor
           gc_msg_val_et
           iv_eventtype
      INTO ls_bapiret-message.
    ls_bapiret-type       = gc_msg_type_e.
    ls_bapiret-id         = gc_msg_class.
    ls_bapiret-number     = gc_msg_no_088.
    ls_bapiret-message_v1 = iv_mastertabdef.
    ls_bapiret-message_v2 = iv_extractor.
    ls_bapiret-message_v3 = gc_msg_val_et.
    ls_bapiret-message_v4 = iv_eventtype.
    ls_bapiret-system     = iv_appsys.
    APPEND ls_bapiret TO ct_logtable.
  ENDIF.

ENDFORM.                    " create_logtable_et


*********  New for D2O and LBN scenarios ***********

*&---------------------------------------------------------------------*
*&      Form  populate_tracking_header
*&---------------------------------------------------------------------*
*       Populate common fields of event message header
*----------------------------------------------------------------------*
*      <-> CV_HEADER
*----------------------------------------------------------------------*
FORM populate_tracking_header
    USING    iv_trxcod      TYPE /saptrx/trxcod
             iv_trxid       TYPE /saptrx/trxid
    CHANGING cv_evm_header  TYPE /saptrx/bapi_evm_header
             cv_evtcnt      TYPE /saptrx/evtcnt.

  cv_evtcnt                 = cv_evtcnt + 1.
  cv_evm_header-evtcnt      = cv_evtcnt.

*   Login language
  cv_evm_header-language   = sy-langu.
*   Tracking ID code set
  cv_evm_header-trxcod      =  iv_trxcod.
*   Tracking ID
  cv_evm_header-trxid       =  iv_trxid.
*   Event Date (Date and time, local date of user)
  cv_evm_header-evtdat      = sy-datlo.
*   Event Time (Date and time, local time for user)
  cv_evm_header-evttim      = sy-timlo.
*   Event Time Zone (Date and time, time zone of user)
  cv_evm_header-evtzon      = sy-zonlo.


ENDFORM.
