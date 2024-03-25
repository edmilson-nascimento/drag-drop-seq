*----------------------------------------------------------------------*
*- Report YTESTE
*----------------------------------------------------------------------*
*-
*----------------------------------------------------------------------*
REPORT YTESTE.

tables:
  aufk, afko .

class lcl_application definition deferred.

data:
  go_appl           type ref to lcl_application,
  go_container      type ref to cl_gui_docking_container,
  go_grid           type ref to cl_gui_alv_grid,
  go_grid_behaviour type ref to cl_dragdrop,
  gt_outtab         type MAKT_TAB,
  gt_outtab_display type MAKT_TAB,
  gs_layout         type lvc_s_layo,

  gv_save_ok_code   type sy-ucomm,
  ok_code           type sy-ucomm.

*----------------------------------------------------------------------*
*-       Class LCL_APPLICATION
*----------------------------------------------------------------------*
class lcl_application definition.

  public section .

    methods handle_grid_drag
                  for event ondrag of cl_gui_alv_grid
      importing es_row_no e_column e_dragdropobj .

    methods handle_grid_drop
                  for event ondrop of cl_gui_alv_grid
      importing e_row e_column e_dragdropobj .

    methods handle_grid_drop_complete
      for event ondropcomplete of
          cl_gui_alv_grid
      importing
          e_row e_column e_dragdropobj .

endclass.                " LCL_APPLICATION


**----------------------------------------------------------------------*
**-       Class DRAG_DROP_OBJECT
**----------------------------------------------------------------------*
**        Text
**----------------------------------------------------------------------*
class lcl_drag_drop_object definition.

  public section .

    data:
      gs_line_aux  type makt,
      gs_index_aux type i.

endclass.               "DRAG_DROP_OBJECT


*----------------------------------------------------------------------*
*-       Class (Implementation)  application
*----------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_application implementation.

  method handle_grid_drag.

    data:
      lo_data_object type ref to lcl_drag_drop_object,
      lo_row_aux     like line of gt_outtab.                "#EC NEEDED

    read table gt_outtab_display into lo_row_aux index es_row_no-row_id .
    if ( sy-subrc eq 0 ) .

      create object lo_data_object .

      if ( lo_data_object is bound ) .

        move es_row_no-row_id to lo_data_object->gs_index_aux .

        read table gt_outtab_display into lo_data_object->gs_line_aux
             index es_row_no-row_id .
        if ( sy-subrc eq 0 ) .
          e_dragdropobj->object = lo_data_object.
        endif .

      endif .

    endif .

  endmethod.                    "handle_grid_drag


* implement what should happen when sth. will be dropped
  method handle_grid_drop.

    data:
      data_object type ref to lcl_drag_drop_object .

    catch system-exceptions move_cast_error = 1.

      data_object ?= e_dragdropobj->object.

      if e_dragdropobj->effect = cl_dragdrop=>move.
        delete gt_outtab_display index data_object->gs_index_aux.
        insert data_object->gs_line_aux into gt_outtab_display index e_row-index.
      endif.
    endcatch.

  endmethod.                    "handle_grid_drop


  method handle_grid_drop_complete.

    if e_dragdropobj->effect = cl_dragdrop=>copy.
    endif.

    call method go_grid->refresh_table_display.

    if sy-subrc <> 0.
      call method e_dragdropobj->abort.
    endif.

  endmethod.                    "handle_grid_drop_complete

endclass.               "application

*----------------------------------------------------------------------
* Modules
*----------------------------------------------------------------------
*----------------------------------------------------------------------*
*-      Module  STATUS_0100  OUTPUT
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status 'STATUS_0100' of program 'BCALV_TEST_DRAG_DROP_02' .
  set titlebar  'STATUS_0100' of program 'BCALV_TEST_DRAG_DROP_02'.

  if ( go_grid is initial ) .
    perform create_controls.
  endif.

endmodule.                 " STATUS_0100  OUTPUT

*----------------------------------------------------------------------*
*-      Module  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  gv_save_ok_code = ok_code.
  clear ok_code.

* check the functions' code after your input
  case gv_save_ok_code.
    when 'EXIT' or 'BACK'.
      if not go_container is initial.
        call method go_container->free
          exceptions
            cntl_system_error = 1
            cntl_error        = 2.
        if sy-subrc <> 0.
*          message a000.
        endif.
        call method cl_gui_cfw=>flush
          exceptions
            cntl_system_error = 1
            cntl_error        = 2.
        if sy-subrc <> 0.
*          message a000.
        endif.
        if gv_save_ok_code = 'EXIT'.
          leave program.
        else.
          call selection-screen 1000.
        endif.
      endif.
  endcase.

  clear gv_save_ok_code.

endmodule.                 " USER_COMMAND_0100  INPUT

*----------------------------------------------------------------------
* Subroutines
*----------------------------------------------------------------------
form create_controls .

* creation of the ALV Grid Control via a docking container
  create object go_container
    exporting
      dynnr     = '100'
      extension = 312
      side      = cl_gui_docking_container=>dock_at_top.

  create object go_grid
    exporting
      i_parent = go_container.

  create object go_appl.

* registrate the methods
  set handler go_appl->handle_grid_drag for go_grid.
  set handler go_appl->handle_grid_drop for go_grid.
  set handler go_appl->handle_grid_drop_complete for go_grid.

  perform build_and_assign_handler.


  if ( lines( gt_outtab ) eq 0 ) .
    select *
      from aufk
      into corresponding fields of table gt_outtab
     up to 20 rows order by primary key.                "#EC CI_NOWHERE
  endif .
  if ( lines( gt_outtab_display ) eq 0 ) .
    append lines of gt_outtab to gt_outtab_display .
  endif .


  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name = 'MAKT'
      is_layout        = gs_layout
    changing
      it_outtab        = gt_outtab_display.


endform.                    " create_controls


*----------------------------------------------------------------------*
*-      Form  build_and_assign_handler
*----------------------------------------------------------------------*
form build_and_assign_handler .

  data:
    lv_effect_move type i,
    lv_handle_grid type i.

  create object go_grid_behaviour .

  if ( go_grid_behaviour is bound ) .
    lv_effect_move = cl_dragdrop=>move.

    go_grid_behaviour->ADD(
      exporting
        flavor         = 'LINE'
        dragsrc        = 'X'
        droptarget     = 'X'
        effect_in_ctrl = lv_effect_move
      exceptions
        already_defined = 1
        obj_invalid     = 2
        others          = 3
     ) .

    if ( sy-subrc ne 0 ) .
*     message id sy-msgid type sy-msgty number sy-msgno
*       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    go_grid_behaviour->get_handle(
      importing
        handle = lv_handle_grid
    ) .

    gs_layout-s_dragdrop-row_ddid = lv_handle_grid.

  endif .

endform.                    " build_and_assign_handler
**----------------------------------------------------------------------
** Selection screen
**----------------------------------------------------------------------
*select-options:
*  s_date   for sy-datum,
*  s_werks  for aufk-werks,
*  s_auart  for aufk-auart,
*  s_astnr  for aufk-astnr,
*  s_aufnr  for aufk-aufnr,
*  s_plnbez for afko-plnbez .

*----------------------------------------------------------------------
* Events
*----------------------------------------------------------------------
initialization .

  select *
   up to 4 rows
    from makt
    into table gt_outtab .

  if ( sy-subrc eq 0 ) .
    append lines of gt_outtab to gt_outtab_display .
  endif .

  "set screen 100.
  call screen 100 .
