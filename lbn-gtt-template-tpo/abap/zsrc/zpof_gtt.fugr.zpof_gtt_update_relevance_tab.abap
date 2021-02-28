FUNCTION zpof_gtt_update_relevance_tab.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_RELEVANCE) TYPE  ZPOF_GTT_EE_REL
*"----------------------------------------------------------------------

  MODIFY zpof_gtt_ee_rel FROM is_relevance.

*  COMMIT WORK.

ENDFUNCTION.
