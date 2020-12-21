*&---------------------------------------------------------------------*
*& Local class definition - Factories
*&---------------------------------------------------------------------*

**********************************************************************
*** Freight Order Header ******************************************
**********************************************************************

CLASS lcl_tor_factory DEFINITION
  INHERITING FROM lcl_factory.

  PUBLIC SECTION.
    METHODS lif_factory~get_bo_reader REDEFINITION.

    METHODS lif_factory~get_pe_filler REDEFINITION.

ENDCLASS.

CLASS lcl_tor_factory IMPLEMENTATION.
  METHOD lif_factory~get_bo_reader.

    ASSIGN is_appl_object-maintabref->* TO FIELD-SYMBOL(<ls_tor_root>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT /scmtms/if_tor_c=>sc_node_attribute-root-tor_cat
        OF STRUCTURE <ls_tor_root> TO FIELD-SYMBOL(<lv_tor_cat>).
      IF sy-subrc = 0.
        CASE <lv_tor_cat>.
          WHEN /scmtms/if_tor_const=>sc_tor_category-active.
            ro_bo_reader = NEW lcl_bo_freight_order_reader( io_ef_parameters ).
          WHEN /scmtms/if_tor_const=>sc_tor_category-booking.
            ro_bo_reader = NEW lcl_bo_freight_booking_reader( io_ef_parameters ).
          WHEN OTHERS.
            MESSAGE i009(zsst_gtt) WITH <lv_tor_cat> INTO DATA(lv_dummy).
            lcl_tools=>throw_exception( ).
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD lif_factory~get_pe_filler.
    ro_pe_filler    = NEW lcl_pe_filler_fo_header(
                        io_ef_parameters = io_ef_parameters
                        io_bo_reader     = io_bo_reader ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Deletion of Purchasing Order Item ********************************
**********************************************************************
CLASS lcl_ae_factory_fo_arrival DEFINITION
  INHERITING FROM lcl_ae_factory.

  PUBLIC SECTION.
    METHODS lif_ae_factory~get_ae_filler REDEFINITION.

ENDCLASS.

CLASS lcl_ae_factory_fo_arrival IMPLEMENTATION.
  METHOD lif_ae_factory~get_ae_filler.
    ro_ae_filler    = NEW lcl_ae_filler_fo_arrival(
                        io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.
