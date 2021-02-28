*----------------------------------------------------------------------*
***INCLUDE LZGTT_SOFF03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form read_appl_tables_status
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_XVBUK
*&      --> LT_YVBUK
*&      --> I_ALL_APPL_TABLES
*&---------------------------------------------------------------------*
FORM read_appl_tables_status
TABLES ct_xvbuk STRUCTURE vbukvb
  ct_yvbuk STRUCTURE vbukvb
USING  i_all_appl_tables TYPE trxas_tabcontainer.

  DATA:
*   Container with references
        ls_one_app_tables   TYPE trxas_tabcontainer_wa.

  FIELD-SYMBOLS:
*   Sales Document: Header Status and Admin Data New
  <lt_xvbuk> TYPE STANDARD TABLE,
*   Sales Document: Header Status and Admin Data Old
  <lt_yvbuk> TYPE STANDARD TABLE.

* <1-1> Sales Document: Header Status and Admin Data New
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'DELIVERY_HDR_STATUS_NEW'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_xvbuk>.
    ct_xvbuk[] = <lt_xvbuk>[].
    SORT ct_xvbuk BY vbeln.
  ENDIF.

* <1-2> Sales Document: Header Status and Admin Data Old
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'DELIVERY_HDR_STATUS_OLD'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_yvbuk>.
    ct_yvbuk[] = <lt_yvbuk>[].
    SORT ct_yvbuk BY vbeln.
  ENDIF.

ENDFORM.                    " read_appl_tables_status

*&---------------------------------------------------------------------*
*& Form read_appl_tables_delivery_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_XVBUK
*&      --> LT_YVBUK
*&      --> I_ALL_APPL_TABLES
*&---------------------------------------------------------------------*
FORM read_appl_tables_delivery_item
TABLES ct_xvlips STRUCTURE lipsvb
       ct_yvlips STRUCTURE lipsvb
USING  i_all_appl_tables TYPE trxas_tabcontainer.

  DATA:
*   Container with references
        ls_one_app_tables   TYPE trxas_tabcontainer_wa.

  FIELD-SYMBOLS:
*   Sales Document: Header Status and Admin Data New
  <lt_xvlips> TYPE STANDARD TABLE,
*   Sales Document: Header Status and Admin Data Old
  <lt_yvlips> TYPE STANDARD TABLE.

* <1-1> Sales Document: Delivery Item Data New
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'DELIVERY_ITEM_NEW'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_xvlips>.
    ct_xvlips[] = <lt_xvlips>[].
    SORT ct_xvlips BY vbeln posnr.
  ENDIF.

* <1-2> Sales Document: Delivery Item Data Old
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'DELIVERY_ITEM_OLD'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_yvlips>.
    ct_yvlips[] = <lt_yvlips>[].
    SORT ct_yvlips BY vbeln posnr.
  ENDIF.

ENDFORM.                    " read_appl_tables_status

*&---------------------------------------------------------------------*
*& Form read_appl_tables_packing_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_XVBUK
*&      --> LT_YVBUK
*&      --> I_ALL_APPL_TABLES
*&---------------------------------------------------------------------*
FORM read_appl_tables_packing_item
TABLES ct_xvepo STRUCTURE vepovb
       ct_yvepo STRUCTURE vepovb
USING  i_all_appl_tables TYPE trxas_tabcontainer.

  DATA:
*   Container with references
        ls_one_app_tables   TYPE trxas_tabcontainer_wa.

  FIELD-SYMBOLS:
*   Sales Document: Header Status and Admin Data New
  <lt_xvepo> TYPE STANDARD TABLE,
*   Sales Document: Header Status and Admin Data Old
  <lt_yvepo> TYPE STANDARD TABLE.

* <1-1> Sales Document: Delivery Item Data New
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'HU_ITEM_NEW'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_xvepo>.
    ct_xvepo[] = <lt_xvepo>[].
    SORT ct_xvepo BY vbeln posnr.
  ENDIF.

* <1-2> Sales Document: Delivery Item Data Old
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'HU_ITEM_OLD'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_yvepo>.
    ct_yvepo[] = <lt_yvepo>[].
    SORT ct_yvepo BY vbeln posnr.
  ENDIF.

ENDFORM.                    " read_appl_tables_status

*&---------------------------------------------------------------------*
*& Form read_appl_tables_packing_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_XVBUK
*&      --> LT_YVBUK
*&      --> I_ALL_APPL_TABLES
*&---------------------------------------------------------------------*
FORM read_appl_tables_pod_items
TABLES ct_xvbup STRUCTURE vbupvb
       ct_yvbup STRUCTURE vbupvb
USING  i_all_appl_tables TYPE trxas_tabcontainer.

  DATA:
*   Container with references
        ls_one_app_tables   TYPE trxas_tabcontainer_wa.

  FIELD-SYMBOLS:
*   Sales Document: Header Status and Admin Data New
  <lt_xvbup> TYPE STANDARD TABLE,
*   Sales Document: Header Status and Admin Data Old
  <lt_yvbup> TYPE STANDARD TABLE.

* <1-1> Sales Document: Delivery Item Data New
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'DELIVERY_ITEM_STATUS_NEW'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_xvbup>.
    ct_xvbup[] = <lt_xvbup>[].
    SORT ct_xvbup BY vbeln posnr.
  ENDIF.

* <1-2> Sales Document: Delivery Item Data Old
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'DELIVERY_ITEM_STATUS_OLD'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_yvbup>.
    ct_yvbup[] = <lt_yvbup>[].
    SORT ct_yvbup BY vbeln posnr.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form read_appl_tables_shipment_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_XVBUK
*&      --> LT_YVBUK
*&      --> I_ALL_APPL_TABLES
*&---------------------------------------------------------------------*
FORM read_appl_tables_shipmt_header
TABLES ct_xvttk STRUCTURE vttkvb
       ct_yvttk STRUCTURE vttkvb
USING  i_all_appl_tables TYPE trxas_tabcontainer.

  DATA:
*   Container with references
        ls_one_app_tables   TYPE trxas_tabcontainer_wa.

  FIELD-SYMBOLS:
  <lt_xvttk> TYPE STANDARD TABLE,
  <lt_yvttk> TYPE STANDARD TABLE.

* <1-1> Sales Document: Shipment Header Data New
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'SHIPMENT_HEADER_NEW'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_xvttk>.
    ct_xvttk[] = <lt_xvttk>[].
    SORT ct_xvttk BY tknum.
  ENDIF.

* <1-2> Sales Document: Shipment Header Data Old
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'SHIPMENT_HEADER_OLD'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_yvttk>.
    ct_yvttk[] = <lt_yvttk>[].
    SORT ct_yvttk BY tknum.
  ENDIF.

ENDFORM.                    " read_appl_tables_status



*&---------------------------------------------------------------------*
*& Form read_appl_tables_shipment_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_XVBUK
*&      --> LT_YVBUK
*&      --> I_ALL_APPL_TABLES
*&---------------------------------------------------------------------*
FORM read_appl_tables_shipment_leg
TABLES ct_xvtts STRUCTURE vttsvb
       ct_yvtts STRUCTURE vttsvb
USING  i_all_appl_tables TYPE trxas_tabcontainer.

  DATA:
*   Container with references
        ls_one_app_tables   TYPE trxas_tabcontainer_wa.

  FIELD-SYMBOLS:
  <lt_xvtts> TYPE STANDARD TABLE,
  <lt_yvtts> TYPE STANDARD TABLE.

* <1-1> Sales Document: Shipment Header Data New
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'SHIPMENT_LEG_NEW'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_xvtts>.
    ct_xvtts[] = <lt_xvtts>[].
    SORT ct_xvtts BY tknum tsnum.
  ENDIF.

* <1-2> Sales Document: Shipment Header Data Old
  READ TABLE i_all_appl_tables INTO ls_one_app_tables
  WITH KEY tabledef = 'SHIPMENT_LEG_OLD'
  BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    RAISE idata_determination_error.
  ELSE.
    ASSIGN ls_one_app_tables-tableref->* TO <lt_yvtts>.
    ct_yvtts[] = <lt_yvtts>[].
    SORT ct_yvtts BY tknum tsnum.
  ENDIF.

ENDFORM.                    " read_appl_tables_status
