REPORT ztrcktrsr_class_inspector.

PARAMETERS p_clas TYPE seoclsname DEFAULT 'CL_GUI_CONTAINER'.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_include,
             repid    TYPE syrepid,
             descr    TYPE c LENGTH 60,
             exposure TYPE icon_d,
           END OF ts_include,
           tt_includes TYPE STANDARD TABLE OF ts_include WITH EMPTY KEY.

    METHODS on_double_click
                FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
    METHODS constructor
      IMPORTING
        container_grid TYPE REF TO cl_gui_container
        container_code TYPE REF TO cl_gui_container.
    METHODS display.
    METHODS editor.
    METHODS start
      IMPORTING
        i_class TYPE clike.

  PROTECTED SECTION.
    DATA mt_redef          TYPE STANDARD TABLE OF seoredef.
    DATA container_grid    TYPE REF TO cl_gui_container.
    DATA container_code    TYPE REF TO cl_gui_container.
    DATA mo_editor         TYPE REF TO cl_gui_abapedit.
    DATA class_includes    TYPE tt_includes.
    DATA grid              TYPE REF TO cl_salv_table.
    DATA classname         TYPE seoclsname.
    METHODS display_source
      IMPORTING
        include TYPE programm.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.

    me->container_grid = container_grid.
    me->container_code = container_code.

  ENDMETHOD.

  METHOD on_double_click.

    editor( ).
    DATA(source) = class_includes[ row ].
    display_source( source-repid ).

  ENDMETHOD.

  METHOD display_source.

    DATA lt_source TYPE STANDARD TABLE OF string.
    READ REPORT include INTO lt_source.
    mo_editor->set_text( lt_source ).

  ENDMETHOD.

  METHOD editor.

    CHECK container_code IS BOUND.
    CHECK mo_editor IS INITIAL.

    mo_editor = NEW #( parent = container_code ).
    mo_editor->set_readonly_mode( 1 ).

  ENDMETHOD.

  METHOD display.

    IF grid IS INITIAL.

      TRY.
          " create SALV
          cl_salv_table=>factory(
            EXPORTING
              r_container  = container_grid
            IMPORTING
              r_salv_table = grid
            CHANGING
              t_table      = class_includes ).

          grid->get_functions( )->set_all( ).
          grid->get_display_settings( )->set_list_header( CONV #( classname ) ).

          DATA(columns) = grid->get_columns( ).
          CAST cl_salv_column_table( columns->get_column( 'EXPOSURE' ) )->set_icon( abap_true ).
          columns->get_column( 'DESCR' )->set_medium_text( 'Description' ).
          columns->get_column( 'EXPOSURE' )->set_medium_text( 'Exposure' ).

          " register event DOUBLE_CLICK
          SET HANDLER on_double_click FOR grid->get_event( ).

          grid->display( ).
        CATCH cx_salv_error INTO DATA(error).
          MESSAGE error TYPE 'I'.
      ENDTRY.
    ELSE.
      grid->refresh( ).
    ENDIF.

  ENDMETHOD.

  METHOD start.

    DATA textpool TYPE table_of_textpool.
    DATA description TYPE string.

    CHECK classname <> i_class.
    classname = i_class.

    TRY.
        DATA(classinfo) = cl_oo_class=>get_instance( i_class ).
      CATCH cx_class_not_existent INTO DATA(error).
        MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    DATA(includes) = cl_oo_classname_service=>get_all_class_includes( class_name = i_class ).
    CLEAR class_includes.
    LOOP AT includes INTO DATA(include).
      READ TEXTPOOL include INTO textpool LANGUAGE sy-langu.

      IF include+30(1) = 'C'.

        IF include+30(2) = 'CM'.
          cl_oo_classname_service=>get_method_by_include(
            EXPORTING
              incname             = include
            RECEIVING
              mtdkey              = DATA(method)
            EXCEPTIONS
              class_not_existing  = 1
              method_not_existing = 2
              OTHERS              = 3 ).
          IF sy-subrc = 0.
            description = method-cpdname.
            TRY.
                DATA(methodexposure) = classinfo->get_component_exposure( cpdname = method-cpdname ).
              CATCH cx_component_not_existing ##no_handler.
            ENDTRY.
          ENDIF.
        ELSE.
          CASE include+30(5).
            WHEN 'CCDEF'.
              description = 'Local type definitions'.
            WHEN 'CCMAC'.
              description = 'Macros'.
            WHEN 'CCIMP'.
              description = 'Local class implementations'.
            WHEN 'CCAU'.
              description = 'Unit tests'.
            WHEN 'CU'.
              description = 'Class definition/ Public section'.
            WHEN 'CO'.
              description = 'Protected section'.
            WHEN 'CI'.
              description = 'Private Section'.
            WHEN 'CP'.
              description = 'Class pool'.
            WHEN 'CT'.
              description = ''.
            WHEN 'CL'.
              description = 'Local classes'.
            WHEN 'CS'.
              description = 'Complete source code'.

          ENDCASE.
        ENDIF.
      ENDIF.

      APPEND VALUE #(
                 repid    = include
                 descr    = description
                 exposure = SWITCH #( methodexposure
                              WHEN seoc_exposure_private   THEN icon_led_red
                              WHEN seoc_exposure_protected THEN icon_led_yellow
                              WHEN seoc_exposure_public    THEN icon_led_green )
          ) TO class_includes.
    ENDLOOP.

    display( ).

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  DATA(docker) = NEW cl_gui_docking_container(
    side  = cl_gui_docking_container=>dock_at_bottom
    ratio = 95 ).
  DATA(splitter) = NEW cl_gui_easy_splitter_container(
    parent      = docker
    orientation = cl_gui_easy_splitter_container=>orientation_horizontal ).

  DATA(container_grid) = splitter->top_left_container.
  DATA(container_code) = splitter->bottom_right_container.

  DATA(main) = NEW lcl_main(
    container_grid = container_grid
    container_code = container_code ).

AT SELECTION-SCREEN.
  main->start( p_clas ).
