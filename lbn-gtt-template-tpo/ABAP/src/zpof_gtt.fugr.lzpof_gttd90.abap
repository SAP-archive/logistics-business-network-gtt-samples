*&---------------------------------------------------------------------*
*& Local class definition - Factories
*&---------------------------------------------------------------------*

**********************************************************************
*** Purchasing Order Header ******************************************
**********************************************************************
CLASS lcl_factory_po_header DEFINITION
  INHERITING FROM lcl_factory.

  PUBLIC SECTION.
    METHODS lif_factory~get_bo_reader REDEFINITION.

    METHODS lif_factory~get_pe_filler REDEFINITION.

ENDCLASS.

CLASS lcl_factory_po_header IMPLEMENTATION.
  METHOD lif_factory~get_bo_reader.
    ro_bo_reader    = NEW lcl_bo_reader_po_header(
                        io_ef_parameters = io_ef_parameters ).
  ENDMETHOD.

  METHOD lif_factory~get_pe_filler.
    MESSAGE E010(ZPOF_GTT) WITH lif_pof_constants=>cs_tabledef-po_header_new
      INTO DATA(lv_dummy).
    lcl_tools=>throw_exception( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Purchasing Order Item ********************************************
**********************************************************************
CLASS lcl_factory_po_item DEFINITION
  INHERITING FROM lcl_factory.

  PUBLIC SECTION.
    METHODS lif_factory~get_bo_reader REDEFINITION.

    METHODS lif_factory~get_pe_filler REDEFINITION.

ENDCLASS.

CLASS lcl_factory_po_item IMPLEMENTATION.
  METHOD lif_factory~get_bo_reader.
    ro_bo_reader    = NEW lcl_bo_reader_po_item(
                        io_ef_parameters = io_ef_parameters ).
  ENDMETHOD.

  METHOD lif_factory~get_pe_filler.
    ro_pe_filler    = NEW lcl_pe_filler_po_item(
                        io_ef_parameters = io_ef_parameters
                        io_bo_reader     = io_bo_reader ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Inbound Delivery Header ******************************************
**********************************************************************
CLASS lcl_factory_dl_header DEFINITION
  INHERITING FROM lcl_factory.

  PUBLIC SECTION.
    METHODS lif_factory~get_bo_reader REDEFINITION.

    METHODS lif_factory~get_pe_filler REDEFINITION.

ENDCLASS.

CLASS lcl_factory_dl_header IMPLEMENTATION.
  METHOD lif_factory~get_bo_reader.
    ro_bo_reader    = NEW lcl_bo_reader_dl_header(
                        io_ef_parameters = io_ef_parameters ).
  ENDMETHOD.

  METHOD lif_factory~get_pe_filler.
    MESSAGE E010(ZPOF_GTT) WITH lif_pof_constants=>cs_tabledef-dl_header_new
      INTO DATA(lv_dummy).
    lcl_tools=>throw_exception( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Inbound Delivery Item ********************************************
**********************************************************************
CLASS lcl_factory_dl_item DEFINITION
  INHERITING FROM lcl_factory.

  PUBLIC SECTION.
    METHODS lif_factory~get_bo_reader REDEFINITION.

    METHODS lif_factory~get_pe_filler REDEFINITION.

ENDCLASS.

CLASS lcl_factory_dl_item IMPLEMENTATION.
  METHOD lif_factory~get_bo_reader.
    ro_bo_reader    = NEW lcl_bo_reader_dl_item(
                        io_ef_parameters = io_ef_parameters ).
  ENDMETHOD.

  METHOD lif_factory~get_pe_filler.
    ro_pe_filler    = NEW lcl_pe_filler_dl_item(
      io_ef_parameters = io_ef_parameters
      io_bo_reader     = io_bo_reader ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Confirmation of Purchasing Order Item ****************************
**********************************************************************
CLASS lcl_ae_factory_po_item_conf DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_po_item_conf IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_po_item_conf(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Goods Receipt of Purchasing Order Item ***************************
**********************************************************************
CLASS lcl_ae_factory_po_item_gr DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_po_item_gr IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_po_item_gr(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Deletion of Purchasing Order Item ********************************
**********************************************************************
CLASS lcl_ae_factory_po_item_del DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_po_item_del IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_po_item_del(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Put Away of Inbound Delivery Item ********************************
**********************************************************************
CLASS lcl_ae_factory_dl_item_pa DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_dl_item_pa IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_dl_item_pa(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Packing of Inbound Delivery Item *********************************
**********************************************************************
CLASS lcl_ae_factory_dl_item_pkng DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_dl_item_pkng IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_dl_item_pkng(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Goods Receipt of Inbound Delivery Item ***************************
**********************************************************************
CLASS lcl_ae_factory_dl_item_gr DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_dl_item_gr IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_dl_item_gr(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Shipment Header **************************************************
**********************************************************************
CLASS lcl_factory_sh_header DEFINITION
  INHERITING FROM lcl_factory.

  PUBLIC SECTION.
    METHODS lif_factory~get_bo_reader REDEFINITION.

    METHODS lif_factory~get_pe_filler REDEFINITION.

ENDCLASS.

CLASS lcl_factory_sh_header IMPLEMENTATION.
  METHOD lif_factory~get_bo_reader.
    ro_bo_reader    = NEW lcl_bo_reader_sh_header(
                        io_ef_parameters = io_ef_parameters ).
  ENDMETHOD.

  METHOD lif_factory~get_pe_filler.
    ro_pe_filler    = NEW lcl_pe_filler_sh_item(
                        io_ef_parameters = io_ef_parameters
                        io_bo_reader     = io_bo_reader ).
  ENDMETHOD.
ENDCLASS.

***********************************************************************
**** Check In of Shipment Header **************************************
***********************************************************************
CLASS lcl_ae_factory_sh_header_ci DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_sh_header_ci IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_sh_header_ci(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

***********************************************************************
**** Load Start of Shipment Header ************************************
***********************************************************************
CLASS lcl_ae_factory_sh_header_ls DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_sh_header_ls IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_sh_header_ls(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

***********************************************************************
**** Load End of Shipment Header **************************************
***********************************************************************
CLASS lcl_ae_factory_sh_header_le DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_sh_header_le IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_sh_header_le(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

***********************************************************************
**** Departure of Shipment Header *************************************
***********************************************************************
CLASS lcl_ae_factory_sh_header_dep DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_sh_header_dep IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_sh_header_dep(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.

***********************************************************************
**** Arrival of Shipment Header ***************************************
***********************************************************************
CLASS lcl_ae_factory_sh_header_arr DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_sh_header_arr IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_sh_header_arr(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.
