*&---------------------------------------------------------------------*
*& Report ytest00
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ytest00.

tables:
  aufk, afko .

class application definition deferred.



data move_row type char1 value 'X' .

* data declaration
data: begin of gt_outtab_3 occurs 0.
    include structure sflight.
data: celltab type lvc_t_drdr.
data: end of gt_outtab_3.

data: application       type ref to application,
      container         type ref to cl_gui_docking_container,
      grid              type ref to cl_gui_alv_grid,
      grid_behaviour    type ref to cl_dragdrop,
      ok_code           like sy-ucomm,
      save_ok_code      like ok_code,
      m                 like sy-tabix,
      gt_outtab         type alv_t_t2 occurs 0,
      gt_outtab_display type alv_t_t2 occurs 0,
      gs_outtab         like line of gt_outtab,
      gs_outtab_2       like line of gt_outtab_display,
      fieldcat          type lvc_t_fcat,  " WITH HEADER LINE,
      ls_fieldcat       like line of fieldcat,
      gs_layout         type lvc_s_layo,
      it_sflight        like standard table of sflight with header line,
      ls_celltab        type lvc_s_drdr,
      index_gt_outtab_3 type i,
      index             type i,
      gs_outtab_3       like line of gt_outtab_3.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class APPLICATION
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class application definition.

  public section.

* methods for D&D handling
    methods: handle_grid_drag for event ondrag of cl_gui_alv_grid
      importing es_row_no e_column e_dragdropobj,
      handle_grid_drop for event ondrop of cl_gui_alv_grid
        importing e_row e_column e_dragdropobj,
      handle_grid_drop_complete for event ondropcomplete of
                    cl_gui_alv_grid
        importing e_row e_column e_dragdropobj.

endclass.               "APPLICATION


*&---------------------------------------------------------------------*
*&       Class DRAG_DROP_OBJECT
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class drag_drop_object definition.

  public section.
    data:
      wa_test like line of gt_outtab,
      index   type i.

endclass.               "DRAG_DROP_OBJECT


*&---------------------------------------------------------------------*
*&       Class (Implementation)  application
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class application implementation.

* implement what should happen when sth. will be dragged
  method handle_grid_drag.

    data: data_object type ref to drag_drop_object,
          help_row    like line of gt_outtab.               "#EC NEEDED

    read table gt_outtab_display into help_row index es_row_no-row_id.

    create object data_object.

    move es_row_no-row_id to data_object->index.

    read table gt_outtab_display into data_object->wa_test index
    es_row_no-row_id.

    e_dragdropobj->object = data_object.

  endmethod.                    "handle_grid_drag


* implement what should happen when sth. will be dropped
  method handle_grid_drop.

    data: data_object type ref to drag_drop_object,
          drop_index  type i,
          help_row_2  like line of gt_outtab,
          ls_cells    type lvc_s_pos,
          lt_cells    type lvc_t_pos.


    catch system-exceptions move_cast_error = 1.

      data_object ?= e_dragdropobj->object.

      if e_dragdropobj->effect = cl_dragdrop=>move.
* if you only want to put one row to another place in the table
        if move_row = 'X'.
          delete gt_outtab_display index data_object->index.
          insert data_object->wa_test into gt_outtab_display index e_row-index
          .
** if you want to change the positions of the rows
*        elseif chng_row = 'X'.
*          read table gt_outtab_2 into help_row_2 index e_row-index.
*          delete gt_outtab_2 index e_row-index.
*          delete gt_outtab_2 index data_object->index.
*          insert help_row_2 into gt_outtab_2 index data_object->index.
*          insert data_object->wa_test into gt_outtab_2 index e_row-index
*          .
*        endif.
*      elseif e_dragdropobj->effect = cl_dragdrop=>copy.
** if you want to copy the content of one cell of the column 'STRING_F'
** to another cell of this column
*        if copy_cel = 'X' or data_cel = 'X'..
*          read table gt_outtab_2 into help_row_2 index e_row-index.
*          help_row_2-string_f = data_object->wa_test-string_f.
*          modify gt_outtab_2 from help_row_2 index e_row-index.
*          if data_cel = 'X'.
** create table of cell-ids
*            ls_cells-fieldname = 'STRING_F'.
*            ls_cells-row_id = e_row-index.
*            append ls_cells to lt_cells.
*
** delta-update the grid
*            call method grid->change_data_from_inside
*              exporting
*                it_cells = lt_cells.
*          endif.
        endif.
      endif.

    endcatch.

  endmethod.                    "handle_grid_drop


  method handle_grid_drop_complete.

    if e_dragdropobj->effect = cl_dragdrop=>copy.
*      if chng_col = 'X'.
*        call method grid->get_frontend_fieldcatalog
*          importing
*            et_fieldcatalog = fieldcat.
** if you want to change the positions of the columns in the table
**by d&d'ing one cell of the first column to another cell of the second
**column
*        loop at fieldcat[] into ls_fieldcat where fieldname = 'PRICE'.
*          if ls_fieldcat-col_pos = '4'.
*            ls_fieldcat-col_pos = '10'.
*            modify fieldcat[] from ls_fieldcat.
*          endif.
*          if ls_fieldcat-col_pos = '9'.
*            ls_fieldcat-col_pos = '4'.
*            modify fieldcat[] from ls_fieldcat.
*          endif.
*        endloop.
*        clear ls_fieldcat.
*        loop at fieldcat[] into ls_fieldcat where fieldname =
*        'PAYMENTSUM'.
*          if ls_fieldcat-col_pos = '4'.
*            ls_fieldcat-col_pos = '10'.
*            modify fieldcat[] from ls_fieldcat.
*          endif.
*          if ls_fieldcat-col_pos = '9'.
*            ls_fieldcat-col_pos = '4'.
*            modify fieldcat[] from ls_fieldcat.
*          endif.
*        endloop.
** set the fieldcatalog to make the changes at the backend
*        call method grid->set_frontend_fieldcatalog
*          exporting
*            it_fieldcatalog = fieldcat[].
*
*      endif.
    endif.

**    if data_cel = ' '.
*** refresh the table display to make the changes visible at the frontend
**      call method grid->refresh_table_display.
**    endif.

    call method grid->refresh_table_display.


    if sy-subrc <> 0.
      call method e_dragdropobj->abort.
    endif.

  endmethod.                    "handle_grid_drop_complete

endclass.               "application



class lcl_local_class definition .

  public section .

    types:
      begin of ty_out,
        bp_id         type snwd_bpa-bp_id,
        company_name  type snwd_bpa-company_name,
        currency_code type snwd_bpa-currency_code,
        web_address   type snwd_bpa-web_address,
        email_address type snwd_bpa-email_address,
        country       type snwd_ad-country,
        city          type snwd_ad-city,
        postal_code   type snwd_ad-postal_code,
        street        type snwd_ad-street,
      end of ty_out,

      tab_out     type table of ty_out,
      range_bp_id type range of snwd_bpa-bp_id,
      tab_bpa     type table of snwd_bpa, " Address Table
      tab_ad      type table of snwd_ad.  " Business Partners

    methods search
      importing
        !bp_id   type lcl_local_class=>range_bp_id
      changing
        !bpa_tab type lcl_local_class=>tab_bpa
        !ad_tab  type lcl_local_class=>tab_ad .

    methods process_alv
      importing
        !bpa_tab type lcl_local_class=>tab_bpa
        !ad_tab  type lcl_local_class=>tab_ad
      changing
        !out_tab type ALV_T_GEN .

    methods display_alv
      changing
        !out_tab type ALV_T_GEN .

  protected section .

  private section .

endclass .


class lcl_local_class implementation .

  method search .

*    refresh:
*      bpa_tab, ad_tab .
*
*    if ( lines( bp_id ) eq 0 ) .
*    else .
*
*      select *
*        into table bpa_tab
*        from snwd_bpa
*       where bp_id in bp_id .
*
*      if ( sy-subrc eq 0 ) .
*
*        select *
*          into table ad_tab
*          from snwd_ad
*           for all entries in bpa_tab
*         where node_key eq bpa_tab-address_guid .
*
*        if ( sy-subrc eq 0 ) .
*        endif .
*
*      endif .
*
*    endif .

  endmethod .


  method process_alv .

    data:
      out_line type lcl_local_class=>ty_out .

    refresh out_tab .


    select *
      from alv_t_t2
      into corresponding fields of table out_tab
     up to 50 rows order by primary key.                "#EC CI_NOWHERE


  endmethod .


  method display_alv .


    data application type ref to application .
    create object application.

* registrate the methods
    set handler application->handle_grid_drag for grid.
    set handler application->handle_grid_drop for grid.
    set handler application->handle_grid_drop_complete for grid.



  endmethod .

endclass .
*----------------------------------------------------------------------
* Modules
*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status 'STATUS_0100'.
  set pf-status 'STATUS_0100' of program 'BCALV_TEST_DRAG_DROP_02' .
  set titlebar 'STATUS_0100'.
  set titlebar 'STATUS_0100' of program 'BCALV_TEST_DRAG_DROP_02'.

  if grid is initial.
* creation of all the controls
    perform create_controls.
  endif.

endmodule.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  save_ok_code = ok_code.
  clear ok_code.

* check the functions' code after your input
  case save_ok_code.
    when 'EXIT' or 'BACK'.
      if not container is initial.
        call method container->free
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
        if save_ok_code = 'EXIT'.
          leave program.
        else.
          call selection-screen 1000.
        endif.
      endif.
  endcase.

  clear save_ok_code.

endmodule.                 " USER_COMMAND_0100  INPUT

*----------------------------------------------------------------------
* Subroutines
*----------------------------------------------------------------------
form create_controls .

* creation of the ALV Grid Control via a docking container
  create object container
    exporting
      dynnr     = '100'
      extension = 312
      side      = cl_gui_docking_container=>dock_at_top.

  create object grid
    exporting
      i_parent = container.

  create object application.

* registrate the methods
  set handler application->handle_grid_drag for grid.
  set handler application->handle_grid_drop for grid.
  set handler application->handle_grid_drop_complete for grid.

  perform build_and_assign_handler.

  if ( lines( gt_outtab ) eq 0 ) .
    select * from alv_t_t2 into corresponding fields of table gt_outtab
               up to 20 rows order by primary key.      "#EC CI_NOWHERE
  endif .
  if ( lines( gt_outtab_display ) eq 0 ) .
    append lines of gt_outtab to gt_outtab_display .
  endif .

* selection of the data via the function 'BCALV_TEST_ALV_T_T2_GEN'
* which uses randomized numbers and strings
* (but not for changing the positions of two columns)
* and display them at the frontend
* selection of the data
**  if chng_col = ' '.
**    select * from alv_t_t2 into corresponding fields of table gt_outtab
**               up to pa_rows rows order by primary key.  "#EC CI_NOWHERE
**
**    if pa_car is initial.
**      loop at gt_outtab into gs_outtab.
**        move-corresponding gs_outtab to gs_outtab_2.
**        append gs_outtab_2 to gt_outtab_2.
**      endloop.
**    elseif pa_con is initial.
**      loop at gt_outtab into gs_outtab where carrid = pa_car.
**        move-corresponding gs_outtab to gs_outtab_2.
**        append gs_outtab_2 to gt_outtab_2.
**      endloop.
**    else.
**      loop at gt_outtab into gs_outtab where carrid = pa_car and connid
**      = pa_con.
**        move-corresponding gs_outtab to gs_outtab_2.
**        append gs_outtab_2 to gt_outtab_2.
**      endloop.
**    endif.
**
**  endif.

  "if move_row = 'X' or chng_row = 'X'.
  if move_row = 'X' .
    call method grid->set_table_for_first_display
      exporting
        i_structure_name = 'ALV_T_T2'
        is_layout        = gs_layout
      changing
        it_outtab        = gt_outtab_display.
  endif.

*  if copy_cel = 'X' or data_cel = 'X'.
*    call method grid->set_table_for_first_display
*      exporting
*        is_layout       = gs_layout
*      changing
*        it_outtab       = gt_outtab_2
*        it_fieldcatalog = fieldcat[].
*  endif.
*
*  if chng_col = 'X'.
*    call method grid->set_table_for_first_display
*      exporting
*        i_structure_name = 'SFLIGHT'
*        is_layout        = gs_layout
*      changing
*        it_outtab        = gt_outtab_3[]
*        it_fieldcatalog  = fieldcat[].
*  endif.

endform.                    " create_controls


*&---------------------------------------------------------------------*
*&      Form  build_and_assign_handler
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_and_assign_handler .

  data: effect      type i,
        effect_move type i,
        effect_copy type i,
        handle_grid type i.


* creation of the flavors used for setting the grid, some columns
* or specific cells d&d'able
  "if move_row = 'X' or chng_row = 'X'.
  if move_row = 'X' .
    create object grid_behaviour.
    effect_move = cl_dragdrop=>move.

    call method grid_behaviour->add
      exporting
        flavor         = 'LINE'
        dragsrc        = 'X'
        droptarget     = 'X'
        effect_in_ctrl = effect_move.

    call method grid_behaviour->get_handle
      importing
        handle = handle_grid.

  endif.

**  if copy_cel = 'X' or data_cel = 'X'..
**
**    create object grid_behaviour.
**    effect_copy = cl_dragdrop=>copy.
**
**    call method grid_behaviour->add
**      exporting
**        flavor         = 'LINE'
**        dragsrc        = 'X'
**        droptarget     = 'X'
**        effect_in_ctrl = effect_copy.
**
**    call method grid_behaviour->get_handle
**      importing
**        handle = handle_grid.
**
**    call function 'LVC_FIELDCATALOG_MERGE'
**      exporting
**        i_structure_name = 'ALV_T_T2'
**      changing
**        ct_fieldcat      = fieldcat[].
**
**    clear ls_fieldcat.
**    loop at fieldcat into ls_fieldcat where fieldname = 'STRING_F'.
**      ls_fieldcat-dragdropid = handle_grid.
**      ls_fieldcat-emphasize = 'X'.
**      modify fieldcat from ls_fieldcat.
**    endloop.
**
**  endif.
**
**  if chng_col = 'X'.
**
**    create object grid_behaviour.
**    effect = cl_dragdrop=>copy.
**
**    call method grid_behaviour->add
**      exporting
**        flavor         = 'LINE'
**        dragsrc        = 'X'
**        droptarget     = 'X'
**        effect_in_ctrl = effect.
**
**    call method grid_behaviour->get_handle
**      importing
**        handle = handle_grid.
**
**    call function 'LVC_FIELDCATALOG_MERGE'
**      exporting
**        i_structure_name = 'SFLIGHT'
**      changing
**        ct_fieldcat      = fieldcat[].
**
**    clear ls_fieldcat.
**    loop at fieldcat[] into ls_fieldcat where fieldname = 'PRICE'.
**      ls_fieldcat-emphasize = 'X'.
**      modify fieldcat[] from ls_fieldcat.
**    endloop.
**
**    clear ls_fieldcat.
**    loop at fieldcat[] into ls_fieldcat where fieldname = 'PAYMENTSUM'.
**      ls_fieldcat-emphasize = 'X'.
**      modify fieldcat[] from ls_fieldcat.
**    endloop.
**
**    select * from alv_t_t2 into corresponding fields of table gt_outtab
**               up to pa_rows rows order by primary key.  "#EC CI_NOWHERE
**
**    if pa_car is initial.
**      loop at gt_outtab into gs_outtab.
**        move-corresponding gs_outtab to gs_outtab_3.
**        append gs_outtab_3 to gt_outtab_3.
**      endloop.
**    elseif pa_con is initial.
**      loop at gt_outtab into gs_outtab where carrid = pa_car.
**        move-corresponding gs_outtab to gs_outtab_3.
**        append gs_outtab_3 to gt_outtab_3.
**      endloop.
**    else.
**      loop at gt_outtab into gs_outtab where carrid = pa_car and connid
**      = pa_con.
**        move-corresponding gs_outtab to gs_outtab_3.
**        append gs_outtab_3 to gt_outtab_3.
**      endloop.
**    endif.
**
*** set some cells d&d'able
**    loop at gt_outtab_3 into gs_outtab_3.
**      index_gt_outtab_3 = sy-tabix.
**      index = index_gt_outtab_3 mod 2.
**      if index = 0.
**        ls_celltab-fieldname = 'PRICE'.
**        ls_celltab-dragdropid = handle_grid.
**        append ls_celltab to gs_outtab_3-celltab[].
**        modify gt_outtab_3 from gs_outtab_3.
**      else.
**        ls_celltab-fieldname = 'PAYMENTSUM'.
**        ls_celltab-dragdropid = handle_grid.
**        append ls_celltab to gs_outtab_3-celltab[].
**        modify gt_outtab_3 from gs_outtab_3.
**      endif.
**    endloop.
**
**    gs_layout-s_dragdrop-fieldname = 'CELLTAB'.
**
**  endif.

* set the whole grid d&d'able
  " if move_row = 'X' or chng_row = 'X'.
  if move_row = 'X' .
    gs_layout-s_dragdrop-row_ddid = handle_grid.
  endif.

endform.                    " build_and_assign_handler
*----------------------------------------------------------------------
* Selection screen
*----------------------------------------------------------------------
select-options:
  s_date   for sy-datum,
  s_werks  for aufk-werks,
  s_auart  for aufk-auart,
  s_astnr  for aufk-astnr,
  s_aufnr  for aufk-aufnr,
  s_plnbez for afko-plnbez .

*----------------------------------------------------------------------
* Events
*----------------------------------------------------------------------
initialization .


start-of-selection .


  data:
    filtro      type lcl_local_class=>range_bp_id,
    go_alv_salv type ref to lcl_local_class,
    bpa_table   type lcl_local_class=>tab_bpa,
    ad_table    type lcl_local_class=>tab_ad.

* Essa opcao pode ser substituida por um parametro de selecao
  filtro =
    value #(
     ( sign = 'I' option = 'EQ' low = '0100000000' )
     ( sign = 'I' option = 'EQ' low = '0100000001' )
     ( sign = 'I' option = 'EQ' low = '0100000002' )
     ( sign = 'I' option = 'EQ' low = '0100000003' )
     ( sign = 'I' option = 'EQ' low = '0100000004' )
     ( sign = 'I' option = 'EQ' low = '0100000005' )
    ) .


* Criando objeto
  go_alv_salv = new lcl_local_class( ) .

* Verificando se foi criado o objeto
  if ( go_alv_salv is bound ) .

    go_alv_salv->search(
      exporting
        bp_id   = filtro
      changing
        bpa_tab = bpa_table
        ad_tab  = ad_table
    ).

    go_alv_salv->process_alv(
      exporting
        bpa_tab = bpa_table
        ad_tab  = ad_table
      changing
        out_tab = gt_outtab
    ).

*    go_alv_salv->display_alv(
*      changing
*        out_tab = gt_outtab
*    ).

    set screen 100.

  endif .
