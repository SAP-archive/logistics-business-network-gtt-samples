FUNCTION ZPOF_GTT_UPDATE_RELEVANCE_TAB .
*"--------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_RELEVANCE) TYPE  ZPOF_GTT_EE_REL
*"--------------------------------------------------------------------

  MODIFY zpof_gtt_ee_rel FROM is_relevance.

*  COMMIT WORK.

ENDFUNCTION.
