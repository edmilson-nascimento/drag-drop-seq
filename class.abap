CLASS zcl_ej_demo DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    "! <p class="shorttext synchronized" lang="EN">Constructor</p>
    METHODS constructor.

  PRIVATE SECTION.

    DATA out TYPE REF TO if_oo_adt_classrun_out.

    "! <p class="shorttext synchronized" lang="en">Return current date and time</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_log
      RETURNING VALUE(result) TYPE string .

ENDCLASS.



CLASS zcl_ej_demo IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    out->write( data = 'Hi there.' ).
  ENDMETHOD.

  METHOD get_log.
    result = |Date {  cl_abap_context_info=>get_system_date( ) DATE = USER } time { cl_abap_context_info=>get_system_time( ) }|  .
  ENDMETHOD.

ENDCLASS.