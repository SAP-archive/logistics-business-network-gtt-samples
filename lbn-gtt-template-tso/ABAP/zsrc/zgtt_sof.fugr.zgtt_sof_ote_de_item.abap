FUNCTION zgtt_sof_ote_de_item.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_APPSYS) TYPE  /SAPTRX/APPLSYSTEM
*"     REFERENCE(I_APP_OBJ_TYPES) TYPE  /SAPTRX/AOTYPES
*"     REFERENCE(I_ALL_APPL_TABLES) TYPE  TRXAS_TABCONTAINER
*"     REFERENCE(I_APP_TYPE_CNTL_TABS) TYPE  TRXAS_APPTYPE_TABS
*"     REFERENCE(I_APP_OBJECTS) TYPE  TRXAS_APPOBJ_CTABS
*"  TABLES
*"      E_CONTROL_DATA STRUCTURE  /SAPTRX/CONTROL_DATA
*"      E_LOGTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      CDATA_DETERMINATION_ERROR
*"      TABLE_DETERMINATION_ERROR
*"      STOP_PROCESSING
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Top Include
* TYPE-POOLS:trxas.
*----------------------------------------------------------------------*

  DATA:
    ls_app_objects  TYPE trxas_appobj_ctab_wa,
    ls_control_data TYPE /saptrx/control_data,
*    item status table
    lt_xvbup        TYPE STANDARD TABLE OF vbupvb,
*    business partner table
    lt_xvbpa        TYPE STANDARD TABLE OF vbpavb,
*    dangerous goods
    lv_dgoods       TYPE boolean,
*    lv_timestamp    TYPE tzntstmps,
    lv_tzone        TYPE timezone,
*    door text
    lv_ltort        TYPE t30bt-ltort,
*    warehouse text
    lv_lnumt        TYPE t300t-lnumt,
*    warehouse text / door text
    lv_lgtratxt     TYPE char60,
*    shipping point address number,
    lv_adrnr_shpt	  TYPE adrnr,
*    country iso code
    lv_countryiso   TYPE intca,
*    sd document address information
    ls_sd_addr      TYPE vbadr.

  DATA: BEGIN OF ls_address,
          line_type    TYPE ad_line_tp,
          address_line LIKE adrs-line0,
        END OF ls_address.
  DATA: lt_address LIKE STANDARD TABLE OF ls_address.

  FIELD-SYMBOLS:
    <ls_xlikp> TYPE likpvb,
    <ls_xlips> TYPE lipsvb,
    <ls_xvbup> TYPE vbupvb,
    <ls_xvbpa> TYPE vbpavb.

* <1> Read necessary application tables from table reference
* Read Item Status Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_item_stat_new
    CHANGING lt_xvbup.

* <1> Read Ship-to Party New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_partners_new
    CHANGING lt_xvbpa.

* <2> Fill general data for all control data records
  ls_control_data-appsys     = i_appsys.
  ls_control_data-appobjtype = i_app_obj_types-aotype.
  ls_control_data-language   = sy-langu.

* <3> Loop at application objects for geting Delivery Order Item
*     add the following values which cannot be extracted in EM Data Extraction
*Outbound Delivery No (LIPS-VBELN)
*Outbound Delivery item (LIPS-POSNR)
*Document date (LIKP-BLDAT)
*Material (LIPS-MATNR)
*Material description (LIPS-ARKTX)
*Delivery Quantity (LIPSVB-LFIMG/G_LFIMG)
*Unit Of Measure (LIPS-VRKME)
*EAN/UPC (LIPS-EAN11)
*Planned Delivery date (LIKP-LFDAT)
*Gross Weight (LIPS-BRGEW)
*UOM (LIPS-GEWEI)
*Net Weight (LIPS-NTGEW)
*Volume (LIPS-VOLUM)
*UOM (LIPS-VOLEH)
*Dangerous Goods (LIPS-PROFL, if not null, then "X")
*Picking Status (VBUP-KOSTA)
*Packing Status (VBUP-PKSTA)
*Goods Issue status (VBUP-WBSTA)
*Association Sales Order item
*Warehouse No: LIKP-LGNUM
*Door for warehouse: LIKP-LGTOR
*Warehouse door text: T30BT with LGNUM and LGTOR
*Shipping point / Departure (LIKP-VSTEL)
*Departure location type
*Departure address
*Departure country
*Destination (LIKP-KUNNR)
*Destination location type
*Destination country
*Destination address
*Destination email
*Destination telephone
*Bill of lading (LIKP-BOLNR)
*Incoterms (LIKP-INCO1)
*Incoterms location 1 (LIKP-INCO2_L)
*Incoterms version (LIKP-INCOV)
*     - Actual Business Time
*     - Actual Business Time zone
*     - Actual Technical Datetime
*     - Actual Technical Time zone

  LOOP AT i_app_objects INTO ls_app_objects.

*   Application Object ID
    ls_control_data-appobjid   = ls_app_objects-appobjid.

*   Check if Main table is Delivery Order Item or not.
    IF ls_app_objects-maintabdef <> gc_bpt_delivery_item_new.
      PERFORM create_logtable_aot
        TABLES e_logtable
        USING  ls_app_objects-maintabdef
               space
               i_app_obj_types-controldatafunc
               ls_app_objects-appobjtype
               i_appsys.
      RAISE cdata_determination_error.
    ELSE.
*     Read Main Object Table
      ASSIGN ls_app_objects-maintabref->* TO <ls_xlips>.
    ENDIF.

*   Check if Master table is Delivery Order Header or not.
    IF ls_app_objects-mastertabdef <> gc_bpt_delivery_header_new.
      PERFORM create_logtable_aot
        TABLES e_logtable
        USING  ls_app_objects-mastertabdef
               space
               i_app_obj_types-controldatafunc
               ls_app_objects-appobjtype
               i_appsys.
      RAISE cdata_determination_error.
    ELSE.
*     Read Master Object Table
      ASSIGN ls_app_objects-mastertabref->* TO <ls_xlikp>.
    ENDIF.

    CLEAR ls_control_data-paramindex.

*   Outbound Delivery
    ls_control_data-paramname = gc_cp_yn_de_no.
    ls_control_data-value     = <ls_xlips>-vbeln.
    APPEND ls_control_data TO e_control_data.

*   Outbound Delivery Item
    ls_control_data-paramname = gc_cp_yn_de_item_no.
    ls_control_data-value     = <ls_xlips>-posnr.
    APPEND ls_control_data TO e_control_data.

*   Document Date
    ls_control_data-paramname = gc_cp_yn_doc_date.
    IF <ls_xlikp>-bldat IS NOT INITIAL.
      ls_control_data-value     = <ls_xlikp>-bldat.
    ELSE.
      CLEAR ls_control_data-value.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

*   Material
    ls_control_data-paramname = gc_cp_yn_material_no.
    ls_control_data-value     = <ls_xlips>-matnr.
    APPEND ls_control_data TO e_control_data.

*   Material description
    ls_control_data-paramname = gc_cp_yn_material_txt.
    ls_control_data-value     = <ls_xlips>-arktx.
    APPEND ls_control_data TO e_control_data.

*   Delivery Quantity
    ls_control_data-paramname = gc_cp_yn_quantity.
    ls_control_data-value     = <ls_xlips>-lfimg.
    SHIFT ls_control_data-value LEFT  DELETING LEADING space.
    APPEND ls_control_data TO e_control_data.

*   Unit Of Measure
    ls_control_data-paramname = gc_cp_yn_qty_unit.
    ls_control_data-value     = <ls_xlips>-vrkme.
    APPEND ls_control_data TO e_control_data.

*   EAN/UPC
    ls_control_data-paramname = gc_cp_yn_ean_upc.
    ls_control_data-value     = <ls_xlips>-ean11.
    APPEND ls_control_data TO e_control_data.

*   Planned Delivery date
    ls_control_data-paramname = gc_cp_yn_de_plndelivery_date.
    IF <ls_xlikp>-lfdat IS NOT INITIAL.
      ls_control_data-value     = <ls_xlikp>-lfdat.
    ELSE.
      CLEAR ls_control_data-value.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

*   Gross Weight
    ls_control_data-paramname = gc_cp_yn_de_gross_weight.
    ls_control_data-value     = <ls_xlips>-brgew.
    SHIFT ls_control_data-value LEFT  DELETING LEADING space.
    APPEND ls_control_data TO e_control_data.

*   Unit Of Measure
    ls_control_data-paramname = gc_cp_yn_de_gross_weight_uom.
    ls_control_data-value     = <ls_xlips>-gewei.
    APPEND ls_control_data TO e_control_data.

*   Net Weight
    ls_control_data-paramname = gc_cp_yn_de_net_weight.
    ls_control_data-value     = <ls_xlips>-ntgew.
    SHIFT ls_control_data-value LEFT  DELETING LEADING space.
    APPEND ls_control_data TO e_control_data.

*   Volume
    ls_control_data-paramname = gc_cp_yn_de_vol.
    ls_control_data-value     = <ls_xlips>-volum.
    SHIFT ls_control_data-value LEFT  DELETING LEADING space.
    APPEND ls_control_data TO e_control_data.

*   Unit Of Measure
    ls_control_data-paramname = gc_cp_yn_de_vol_uom.
    ls_control_data-value     = <ls_xlips>-voleh.
    APPEND ls_control_data TO e_control_data.

*   Dangerous Goods
    CLEAR lv_dgoods.
    ls_control_data-paramname = gc_cp_yn_dgoods.
    IF <ls_xlips>-profl IS NOT INITIAL.
      lv_dgoods = 'X'.
    ELSE.
      lv_dgoods = ''.
    ENDIF.
    ls_control_data-value     = lv_dgoods.
    APPEND ls_control_data TO e_control_data.

    READ TABLE lt_xvbup ASSIGNING <ls_xvbup> WITH KEY vbeln = <ls_xlips>-vbeln
                                                      posnr = <ls_xlips>-posnr.

*   Picking Status
    ls_control_data-paramname = gc_cp_yn_de_pick_status.
    IF <ls_xvbup>-kosta IS NOT INITIAL.
      ls_control_data-value     = <ls_xvbup>-kosta.
    ELSE.
      ls_control_data-value     = gc_true.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

*   Packing Status
    ls_control_data-paramname = gc_cp_yn_de_pack_status.
    IF <ls_xvbup>-pksta IS NOT INITIAL.
      ls_control_data-value     = <ls_xvbup>-pksta.
    ELSE.
      ls_control_data-value     = gc_true.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

*   Goods Issue status
    ls_control_data-paramname = gc_cp_yn_de_gi_status.
    IF <ls_xvbup>-wbsta IS NOT INITIAL.
      ls_control_data-value     = <ls_xvbup>-wbsta.
    ELSE.
      ls_control_data-value     = gc_true.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

*   Association Sales Order item
    ls_control_data-paramname =  gc_cp_yn_de_asso_soitem_no.
    CONCATENATE <ls_xlips>-vgbel <ls_xlips>-vgpos INTO ls_control_data-value.
    APPEND ls_control_data TO e_control_data.

*Warehouse No: LIKP-LGNUM
    ls_control_data-paramname =  gc_cp_yn_de_warehouse_no.
    ls_control_data-value     = <ls_xlikp>-lgnum.
    APPEND ls_control_data TO e_control_data.
*Door for warehouse: LIKP-LGTOR
    ls_control_data-paramname =  gc_cp_yn_de_door_no.
    ls_control_data-value     = <ls_xlikp>-lgtor.
    APPEND ls_control_data TO e_control_data.
*Warehouse door text: concatenate T300T-LNUMT '/' T30BT-ltort with LGNUM and LGTOR
    CLEAR lv_ltort.
    SELECT SINGLE ltort INTO lv_ltort FROM t30bt WHERE spras = sy-langu
                                                 AND   lgnum = <ls_xlikp>-lgnum
                                                 AND   lgtor = <ls_xlikp>-lgtor.
    CLEAR lv_lnumt.
    SELECT SINGLE lnumt INTO lv_lnumt FROM t300t WHERE spras = sy-langu
                                                 AND   lgnum = <ls_xlikp>-lgnum.
    CLEAR lv_lgtratxt.
    IF lv_ltort IS NOT INITIAL OR lv_lnumt IS NOT INITIAL.
      CONCATENATE lv_lnumt lv_ltort INTO lv_lgtratxt SEPARATED BY '/'.
    ENDIF.
    ls_control_data-paramname = gc_cp_yn_de_door_txt.
    ls_control_data-value     = lv_lgtratxt.
    APPEND ls_control_data TO e_control_data.

*Shipping point / Departure (LIKP-VSTEL)
    ls_control_data-paramname =  gc_cp_yn_de_shp_pnt.
    ls_control_data-value     = <ls_xlikp>-vstel.
    APPEND ls_control_data TO e_control_data.

*Departure location type
    ls_control_data-paramname =  gc_cp_yn_de_shp_pnt_loctype.
    ls_control_data-value     = 'ShippingPoint'.
    APPEND ls_control_data TO e_control_data.

*Departure address
    CLEAR lv_adrnr_shpt.
    SELECT SINGLE adrnr INTO lv_adrnr_shpt FROM tvst WHERE vstel = <ls_xlikp>-vstel.
    CLEAR: lt_address, ls_address.
    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
      EXPORTING
        address_type            = '1'
        address_number          = lv_adrnr_shpt
      IMPORTING
        address_printform_table = lt_address.
    CLEAR ls_control_data-value.
    LOOP AT lt_address INTO ls_address.
      IF sy-tabix = 1.
        ls_control_data-value = ls_address-address_line.
      ELSE.
        IF ls_address-address_line IS NOT INITIAL.
          CONCATENATE ls_control_data-value ls_address-address_line INTO ls_control_data-value SEPARATED BY '$'.
        ENDIF.
      ENDIF.
    ENDLOOP.
    ls_control_data-paramname =  gc_cp_yn_de_shp_addr.
    APPEND ls_control_data TO e_control_data.
*Departure country
    CLEAR: lv_countryiso, ls_sd_addr.
    CALL FUNCTION 'SD_ADDRESS_GET'
      EXPORTING
        fif_address_number = lv_adrnr_shpt
      IMPORTING
        fes_address        = ls_sd_addr.
    CALL FUNCTION 'COUNTRY_CODE_SAP_TO_ISO'
      EXPORTING
        sap_code = ls_sd_addr-land1
      IMPORTING
        iso_code = lv_countryiso.
    ls_control_data-paramname =  gc_cp_yn_de_shp_countryiso.
    ls_control_data-value     = lv_countryiso.
    APPEND ls_control_data TO e_control_data.

*Destination (LIKP-KUNNR)
    ls_control_data-paramname =  gc_cp_yn_de_dest.
    ls_control_data-value     = <ls_xlikp>-kunnr.
    APPEND ls_control_data TO e_control_data.

*Destination location type
    ls_control_data-paramname =  gc_cp_yn_de_dest_loctype.
    ls_control_data-value     = 'Customer'.
    APPEND ls_control_data TO e_control_data.

*Destination address
    READ TABLE lt_xvbpa ASSIGNING <ls_xvbpa> WITH KEY vbeln = <ls_xlikp>-vbeln
                                                      posnr = '000000'
                                                      parvw = 'WE' BINARY SEARCH.
    CLEAR: lt_address, ls_address.
    IF <ls_xvbpa> IS NOT INITIAL.
      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          address_type            = '1'
          address_number          = <ls_xvbpa>-adrnr
        IMPORTING
          address_printform_table = lt_address.
    ENDIF.
    CLEAR ls_control_data-value.
    LOOP AT lt_address INTO ls_address.
      IF sy-tabix = 1.
        ls_control_data-value = ls_address-address_line.
      ELSE.
        IF ls_address-address_line IS NOT INITIAL.
          CONCATENATE ls_control_data-value ls_address-address_line INTO ls_control_data-value SEPARATED BY '$'.
        ENDIF.
      ENDIF.
    ENDLOOP.
    ls_control_data-paramname =  gc_cp_yn_de_dest_addr.
    APPEND ls_control_data TO e_control_data.
*Destination country, Destination email, Destination telephone
    CLEAR: lv_countryiso, ls_sd_addr.
    IF <ls_xvbpa> IS NOT INITIAL.
      CALL FUNCTION 'SD_ADDRESS_GET'
        EXPORTING
          fif_address_number = <ls_xvbpa>-adrnr
        IMPORTING
          fes_address        = ls_sd_addr.
      CALL FUNCTION 'COUNTRY_CODE_SAP_TO_ISO'
        EXPORTING
          sap_code = ls_sd_addr-land1
        IMPORTING
          iso_code = lv_countryiso.
    ENDIF.
    ls_control_data-paramname =  gc_cp_yn_de_dest_countryiso.
    ls_control_data-value     = lv_countryiso.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname =  gc_cp_yn_de_dest_email.
    ls_control_data-value     = ls_sd_addr-email_addr.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname =  gc_cp_yn_de_dest_tele.
    ls_control_data-value     = ls_sd_addr-telf1.
    APPEND ls_control_data TO e_control_data.

*Bill of lading (LIKP-BOLNR)
    ls_control_data-paramname =  gc_cp_yn_de_bol_no.
    ls_control_data-value     = <ls_xlikp>-bolnr.
    APPEND ls_control_data TO e_control_data.
*Incoterms (LIKP-INCO1)
    ls_control_data-paramname =  gc_cp_yn_de_inco1.
    ls_control_data-value     = <ls_xlikp>-inco1.
    APPEND ls_control_data TO e_control_data.
*Incoterms location 1 (LIKP-INCO2_L)
    ls_control_data-paramname =  gc_cp_yn_de_inco2_l.
    ls_control_data-value     = <ls_xlikp>-inco2_l.
    APPEND ls_control_data TO e_control_data.
*Incoterms version (LIKP-INCOV)
    ls_control_data-paramname =  gc_cp_yn_de_incov.
    ls_control_data-value     = <ls_xlikp>-incov.
    APPEND ls_control_data TO e_control_data.

*   Actual Business Time zone
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    ls_control_data-paramname = gc_cp_yn_act_timezone.
    ls_control_data-value     = lv_tzone.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_act_datetime.
    CONCATENATE '0' sy-datum sy-uzeit INTO ls_control_data-value.
    APPEND ls_control_data TO e_control_data.

*   Actual Technical Datetime & Time zone
    ls_control_data-paramname = gc_cp_yn_acttec_timezone."ACTUAL_TECHNICAL_TIMEZONE
    ls_control_data-value     = lv_tzone.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_acttec_datetime."ACTUAL_TECHNICAL_DATETIME
    CONCATENATE '0' sy-datum sy-uzeit INTO ls_control_data-value.
    APPEND ls_control_data TO e_control_data.

  ENDLOOP.

ENDFUNCTION.
