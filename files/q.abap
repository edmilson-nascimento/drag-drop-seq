REPORT x.

CLASS application DEFINITION DEFERRED.
CLASS local_class DEFINITION DEFERRED.

TYPES:
  ty_out  TYPE zca_s_quermesse_prioridades,
  tab_out TYPE zca_t_quermesse_prioridades.


*>>> BEGIN OF MOD INC2967847 · 2024-03-26 - EX135415

*<<< END OF MOD INC2967847 · End

DATA:
*  gv_move_row       TYPE char1 VALUE 'X',
  application       TYPE REF TO application,
  container         TYPE REF TO cl_gui_docking_container,
  grid              TYPE REF TO cl_gui_alv_grid,
  grid_behaviour    TYPE REF TO cl_dragdrop,
*  ok_code           LIKE sy-ucomm,
*  save_ok_code      LIKE ok_code,
  gt_outtab         TYPE tab_out,
  gt_outtab_display TYPE tab_out,
  gs_layout         TYPE lvc_s_layo.

CLASS application DEFINITION.

  PUBLIC SECTION.

    METHODS handle_grid_drag
        FOR EVENT ondrag OF cl_gui_alv_grid
      IMPORTING
        es_row_no
        e_column
        e_dragdropobj.

    METHODS handle_grid_drop
        FOR EVENT ondrop OF cl_gui_alv_grid
      IMPORTING
        e_row
        e_column
        e_dragdropobj .

    METHODS handle_grid_drop_complete
        FOR EVENT ondropcomplete OF cl_gui_alv_grid
      IMPORTING
        e_row
        e_column
        e_dragdropobj.

  PRIVATE SECTION .

    METHODS update_table
      IMPORTING
        im_old_index TYPE lvc_index
        im_new_index TYPE lvc_index
        im_line      TYPE zca_s_quermesse_prioridades
      CHANGING
        ch_table     TYPE zca_t_quermesse_prioridades .

ENDCLASS.


CLASS drag_drop_object DEFINITION.

  PUBLIC SECTION.
    DATA:
      wa_test TYPE ty_out,
      index   TYPE lvc_index.

ENDCLASS.


CLASS application IMPLEMENTATION.

  METHOD handle_grid_drag.

    DATA:
      data_object TYPE REF TO drag_drop_object .

    data_object = NEW #( ).
    IF ( data_object IS NOT BOUND ) .
      RETURN .
    ENDIF .

    data_object->index = es_row_no-row_id.

    READ TABLE gt_outtab_display INTO data_object->wa_test INDEX
    es_row_no-row_id.

    e_dragdropobj->object = data_object.

  ENDMETHOD.

  METHOD handle_grid_drop.

    DATA data_object TYPE REF TO drag_drop_object.

    CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.

      data_object ?= e_dragdropobj->object.

      IF e_dragdropobj->effect = cl_dragdrop=>move.

*        IF gv_move_row = abap_true.

        me->update_table( EXPORTING im_old_index = data_object->index
                                    im_new_index = e_row-index
                                    im_line      = data_object->wa_test
                          CHANGING  ch_table     = gt_outtab_display ).
*        ENDIF.

      ENDIF.

    ENDCATCH.

  ENDMETHOD.

  METHOD handle_grid_drop_complete.

    grid->refresh_table_display( ).

    IF sy-subrc <> 0.
      e_dragdropobj->abort( ).
    ENDIF.

  ENDMETHOD.


  METHOD update_table .

    IF ( im_old_index IS INITIAL ) OR
      ( im_new_index IS INITIAL ) .
      RETURN .
    ENDIF .

    IF lines( ch_table ) EQ 0 .
      RETURN .
    ENDIF .

    " Remove old one
    DELETE ch_table INDEX im_old_index .
    " Insert new
    INSERT im_line INTO ch_table INDEX im_new_index.

    " Update index
    ch_table = VALUE zca_t_quermesse_prioridades(
      LET lt_temp = ch_table
      IN
      FOR l IN lt_temp
        ( prioridade_new = ( lines( ch_table ) + 1 )
          seq_nr         = l-seq_nr
          prioridade     = l-prioridade
          inc            = l-inc
          descricao_oc   = l-descricao_oc
          label_oc       = l-label_oc
          erdat          = l-erdat
          erzet          = l-erzet
          ernam          = l-ernam
          bc             = l-bc
          data_edicao    = l-data_edicao
          hora_edicao    = l-hora_edicao
          user_edicao    = l-user_edicao
          estat          = l-estat )
    ).


  ENDMETHOD .

ENDCLASS.


CLASS local_class DEFINITION.

  PUBLIC SECTION.

    TYPES tab_out TYPE STANDARD TABLE OF zca_tquermes_pri WITH DEFAULT KEY.
    "! <p class="shorttext synchronized" lang="pt">Busca os dados</p>
    METHODS search.

    "! <p class="shorttext synchronized" lang="pt">Configuraçoes iniciais</p>
    METHODS get_data
      RETURNING VALUE(result) TYPE zca_t_quermesse_prioridades.

    "! <p class="shorttext synchronized" lang="pt">TODO</p>
    METHODS display_alv.

    "! <p class="shorttext synchronized" lang="pt">Set Status e config iniciais</p>
    CLASS-METHODS status_0100 .

    "! <p class="shorttext synchronized" lang="pt">Mantem os comandos de tela</p>
    CLASS-METHODS user_command .

  PRIVATE SECTION.

    DATA out_tab TYPE zca_t_quermesse_prioridades.

    "! <p class="shorttext synchronized" lang="pt">Criar controles</p>
    CLASS-METHODS create_controls .

    "! <p class="shorttext synchronized" lang="pt">Manter atividades</p>
    CLASS-METHODS build_and_assign_handler .

ENDCLASS.


CLASS local_class IMPLEMENTATION.

  METHOD search.

    REFRESH out_tab.

    SELECT * FROM zca_tquermes_pri
      INTO CORRESPONDING FIELDS OF TABLE out_tab.

  ENDMETHOD.

  METHOD get_data.

    result = VALUE zca_t_quermesse_prioridades(
      FOR l IN me->out_tab
        ( seq_nr       = l-seq_nr
          prioridade   = ( lines( result ) + 1 )
          inc          = l-inc
          descricao_oc = l-descricao_oc
          label_oc     = l-label_oc
          erdat        = l-erdat
          erzet        = l-erzet
          ernam        = l-ernam
          bc           = l-bc
          data_edicao  = l-data_edicao
          hora_edicao  = l-hora_edicao
          user_edicao  = l-user_edicao
          estat        = l-estat )
    ).

  ENDMETHOD.

  METHOD display_alv.
  ENDMETHOD.


  METHOD status_0100 .

*   SET PF-STATUS 'STATUS_0100' OF PROGRAM 'BCALV_TEST_DRAG_DROP_02'.
    SET PF-STATUS 'STATUS_0100' .
    SET TITLEBAR  'STATUS_0100' OF PROGRAM 'BCALV_TEST_DRAG_DROP_02'.

    IF grid IS INITIAL.
      local_class=>create_controls( ).
    ENDIF.

  ENDMETHOD .


  METHOD create_controls .
    container = NEW #( dynnr     = '100'
                       extension = 312
                       side      = cl_gui_docking_container=>dock_at_top ).

    grid = NEW #( i_parent = container ).

    application = NEW #( ).

    SET HANDLER application->handle_grid_drag FOR grid.
    SET HANDLER application->handle_grid_drop FOR grid.
    SET HANDLER application->handle_grid_drop_complete FOR grid.

    local_class=>build_and_assign_handler( ).

    IF ( lines( gt_outtab_display ) = 0 ).
      APPEND LINES OF gt_outtab TO gt_outtab_display.
    ENDIF.

    gs_layout-cwidth_opt = abap_on .
*    IF gv_move_row = abap_true.
    grid->set_table_for_first_display( EXPORTING i_structure_name = 'ZCA_S_QUERMESSE_PRIORIDADES'
                                                 is_layout        = gs_layout
                                       CHANGING  it_outtab        = gt_outtab_display ).
*    ENDIF.

  ENDMETHOD.


  METHOD build_and_assign_handler .

    DATA:
      effect_move TYPE i,
      handle_grid TYPE i.

*    IF gv_move_row = abap_true.
    grid_behaviour = NEW #( ).
    effect_move = cl_dragdrop=>move.

    grid_behaviour->add( flavor         = 'LINE'
                         dragsrc        = 'X'
                         droptarget     = 'X'
                         effect_in_ctrl = effect_move ).

    grid_behaviour->get_handle( IMPORTING handle = handle_grid ).

*    ENDIF.

*    IF gv_move_row = abap_true.
    gs_layout-s_dragdrop-row_ddid = handle_grid.
*    ENDIF.

  ENDMETHOD .


  METHOD user_command .

*    CLEAR ok_code.

    BREAK-POINT .

*    CASE save_ok_code.
    CASE sy-ucomm .

      WHEN 'EXIT' OR 'BACK'.
        IF container IS NOT INITIAL.
          container->free( EXCEPTIONS cntl_system_error = 1
                                      cntl_error        = 2 ).
          IF sy-subrc <> 0.
          ENDIF.
          cl_gui_cfw=>flush( EXCEPTIONS cntl_system_error = 1
                                        cntl_error        = 2 ).
          IF sy-subrc <> 0.
          ENDIF.
          IF sy-ucomm = 'EXIT'.
            LEAVE PROGRAM.
          ELSE.
            CALL SELECTION-SCREEN 1000.
          ENDIF.
        ENDIF.

      WHEN 'SAVE' .

      WHEN OTHERS .

    ENDCASE.

*    CLEAR save_ok_code.

  ENDMETHOD .

ENDCLASS.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  local_class=>status_0100( ) .

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  local_class=>user_command( ) .

ENDMODULE.


INITIALIZATION.

START-OF-SELECTION.

  DATA(go_alv_salv) = NEW local_class( ).

  IF ( go_alv_salv IS BOUND ).

    go_alv_salv->search( ).

    gt_outtab = go_alv_salv->get_data( ).

    SET SCREEN 100.

  ENDIF.