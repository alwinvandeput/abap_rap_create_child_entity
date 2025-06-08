CLASS zrgd_abbreviated_name_gen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  "Last change: 5-6-2025 - Parameter it_exclude_names in combination with parameter sequence_number_ind
  "Last change: 7-6-2025 - Only First Character to Upper.
  "Last change: 7-6-2025 - no_abbrev_part_till_index    SO_ITM instead of S_ITEM.
  "Last change: 7-6-2025 - iv_abbrev_right_to_left_ind  SO_ITM instead of S_ITEM.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_text_part,
        text                      TYPE string,
        no_abbrev_ind             TYPE abap_boolean,
        split_by_space_ind        TYPE abap_boolean,
        split_by_camel_case_ind   TYPE abap_boolean,
        Only_First_Char_Upper_ind TYPE abap_boolean,
        split_char                TYPE string,
        no_abbrev_part_till_index TYPE i,
        "TODO rename to concatenate character.
        split_text                TYPE string,

        sequence_number_ind       TYPE abap_boolean,
      END OF ts_text_part,
      tt_text_parts TYPE STANDARD TABLE OF ts_text_part WITH EMPTY KEY.

    METHODS get_name
      IMPORTING it_text_parts               TYPE tt_text_parts
                iv_max_length               TYPE i
                iv_abbrev_right_to_left_ind TYPE abap_boolean DEFAULT abap_false
                iv_snake_case               TYPE abap_boolean DEFAULT abap_false
                it_exclude_names            TYPE string_table OPTIONAL
      RETURNING VALUE(rv_name)              TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_level_text_parts,
        text                TYPE string,
        no_abbrev_ind       TYPE abap_boolean,
        sequence_number_ind TYPE abap_boolean,
        sequence_number     TYPE i,
        level               TYPE i,
      END OF ts_level_text_parts,
      tt_level_text_parts TYPE STANDARD TABLE OF ts_level_text_parts WITH EMPTY KEY.

    METHODS split_to_level_text_parts
      IMPORTING it_text_parts              TYPE tt_text_parts
      RETURNING VALUE(rt_level_text_parts) TYPE tt_level_text_parts.

    METHODS _abbreviate_next_part
      IMPORTING iv_level                    TYPE i
                iv_abbrev_right_to_left_ind TYPE abap_boolean
      CHANGING  ct_level_text_parts         TYPE tt_level_text_parts

      RETURNING VALUE(rv_level_up_ind)      TYPE abap_bool.

    METHODS _concatenate_name
      IMPORTING it_level_text_parts TYPE tt_level_text_parts
                iv_snake_case       TYPE abap_boolean
      RETURNING VALUE(rv_name)      TYPE string.

    METHODS _abbreviate_text
      IMPORTING iv_text        TYPE string
                iv_level       TYPE i
      RETURNING VALUE(rv_text) TYPE string.

ENDCLASS.


CLASS zrgd_abbreviated_name_gen IMPLEMENTATION.

  METHOD get_name.

    DATA(lt_level_text_parts) = split_to_level_text_parts( it_text_parts ).

    DATA(lv_level_up_ind) = abap_false.
    DATA(lv_level) = 0.

    " Abbreviate Levels:
    " 0 = No abbreviate
    " 1 = Abbrevate by removing all Vowels, except the first character
    " 2 = Abbreviate by only keeping the first character.

    lv_level = 1.

    DO.

      DATA(lv_abbreviate_ind) = abap_true.

      IF lv_level_up_ind = abap_false.
        rv_name = _concatenate_name(
          it_level_text_parts = lt_level_text_parts
          iv_snake_case       = iv_snake_case ).

        DATA(lv_length) = strlen( rv_name ).
        IF lv_length <= iv_max_length.

          IF it_exclude_names[] IS NOT INITIAL.

            READ TABLE it_exclude_names[]
              WITH KEY table_line = rv_name
              TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.

            READ TABLE lt_level_text_parts
              WITH KEY sequence_number_ind = abap_true
              ASSIGNING FIELD-SYMBOL(<ls_text_part>).
            ASSERT sy-subrc = 0.

            <ls_text_part>-sequence_number += 1.
            <ls_text_part>-text = condense( CONV string( <ls_text_part>-sequence_number ) ).

            lv_abbreviate_ind = abap_false.

          ELSE.

            EXIT.

          ENDIF.
        ENDIF.

      ELSE.
        lv_level += lv_level.

      ENDIF.

      IF lv_level > 3.
        rv_name = rv_name+0(iv_max_length).
        EXIT.
      ENDIF.

      IF lv_abbreviate_ind = abap_true.

        lv_level_up_ind = _abbreviate_next_part(
          EXPORTING
            iv_level                    = lv_level
            iv_abbrev_right_to_left_ind = iv_abbrev_right_to_left_ind
          CHANGING
            ct_level_text_parts         = lt_level_text_parts ).

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD split_to_level_text_parts.

    DATA lt_temp_level_text_parts LIKE rt_level_text_parts.

    LOOP AT it_text_parts
      ASSIGNING FIELD-SYMBOL(<ls_text_part>).

      DATA(lv_text) = <ls_text_part>-text.

      IF <ls_text_part>-Only_First_Char_Upper_ind = abap_true.
        DATA(lv_char_count) = strlen( lv_text ).
        DATA lv_result_text TYPE string.
        DO lv_char_count TIMES.
          DATA(lv_offset) = sy-index - 1.
          DATA(lv_char) = lv_text+lv_offset(1).
          IF sy-index = 1.
            lv_char = to_upper( lv_char ).
          ELSE.
            lv_char = to_lower( lv_char ).
          ENDIF.
          lv_result_text = lv_result_text && lv_char.
        ENDDO.
        lv_text = lv_result_text.
      ENDIF.

      IF <ls_text_part>-split_by_space_ind = abap_true.
        SPLIT lv_text AT space INTO TABLE DATA(lt_split_parts).

        LOOP AT lt_split_parts
          ASSIGNING FIELD-SYMBOL(<lv_split_part>).

          IF sy-tabix > 1.
            IF <ls_text_part>-split_text IS NOT INITIAL.
              APPEND
                VALUE #( text = <ls_text_part>-split_text )
                 TO lt_temp_level_text_parts.
            ENDIF.
          ENDIF.

          APPEND INITIAL LINE TO lt_temp_level_text_parts
            ASSIGNING FIELD-SYMBOL(<ls_temp_level_text_part>).
          <ls_temp_level_text_part>-text = <lv_split_part>.

        ENDLOOP.


      ELSEIF <ls_text_part>-split_char IS NOT INITIAL.

        SPLIT lv_text AT <ls_text_part>-split_char INTO TABLE lt_split_parts.

        LOOP AT lt_split_parts
          ASSIGNING <lv_split_part>.

          IF sy-tabix > 1.
            IF <ls_text_part>-split_text IS NOT INITIAL.
              APPEND
                VALUE #( text = <ls_text_part>-split_text )
                 TO lt_temp_level_text_parts.
            ENDIF.
          ENDIF.

          APPEND INITIAL LINE TO lt_temp_level_text_parts
            ASSIGNING <ls_temp_level_text_part>.
          <ls_temp_level_text_part>-text = <lv_split_part>.

          IF sy-tabix <= <ls_text_part>-no_abbrev_part_till_index.
            <ls_temp_level_text_part>-no_abbrev_ind = abap_true.
          ENDIF.

        ENDLOOP.

      ELSEIF <ls_text_part>-split_by_camel_case_ind = abap_true.

        DATA(lv_length) = strlen( lv_text ).

        DO lv_length TIMES.

          lv_offset = sy-index - 1.

          CLEAR lv_char.
          lv_char = lv_text+lv_offset(1).

          "Create new part
          IF lv_offset = 0.
            APPEND INITIAL LINE TO lt_temp_level_text_parts
              ASSIGNING <ls_temp_level_text_part>.
          ELSE.
            IF lv_char = to_upper( lv_char ).

              IF <ls_text_part>-split_text IS NOT INITIAL.
                APPEND
                  VALUE #( text = <ls_text_part>-split_text )
                   TO lt_temp_level_text_parts.
              ENDIF.

              APPEND INITIAL LINE TO lt_temp_level_text_parts
                ASSIGNING <ls_temp_level_text_part>.
            ENDIF.
          ENDIF.

          "Add character
          <ls_temp_level_text_part>-text = <ls_temp_level_text_part>-text  && lv_char.

        ENDDO.

      ELSE.

        APPEND INITIAL LINE TO lt_temp_level_text_parts
          ASSIGNING <ls_temp_level_text_part>.

        <ls_temp_level_text_part>-text = lv_text.

      ENDIF.

      LOOP AT lt_temp_level_text_parts
        ASSIGNING <ls_temp_level_text_part>.

        APPEND INITIAL LINE TO rt_level_text_parts
          ASSIGNING FIELD-SYMBOL(<ls_level_text_part>).
        <ls_level_text_part>-text          = <ls_temp_level_text_part>-text.
        IF <ls_temp_level_text_part>-no_abbrev_ind = abap_true.
          <ls_level_text_part>-no_abbrev_ind = <ls_temp_level_text_part>-no_abbrev_ind.
        ELSE.
          <ls_level_text_part>-no_abbrev_ind = <ls_text_part>-no_abbrev_ind.
        ENDIF.
        <ls_level_text_part>-sequence_number_ind = <ls_text_part>-sequence_number_ind.

      ENDLOOP.

      CLEAR lt_temp_level_text_parts.

    ENDLOOP.

  ENDMETHOD.


  METHOD _concatenate_name.

    LOOP AT it_level_text_parts
      ASSIGNING FIELD-SYMBOL(<ls_level_text_part>).

      IF sy-tabix > 1 AND iv_snake_case = abap_true.
        rv_name = rv_name && |_|.
      ENDIF.

      rv_name = rv_name && <ls_level_text_part>-text.

    ENDLOOP.

  ENDMETHOD.


  METHOD _abbreviate_next_part.

    DATA lv_index TYPE i.
    DATA(lv_count) = lines( ct_level_text_parts ).

    IF iv_abbrev_right_to_left_ind = abap_true.
      lv_index = lv_count.
    ELSE.
      lv_index = 1.
    ENDIF.

    DO.
      READ TABLE ct_level_text_parts
        INDEX lv_index
        ASSIGNING FIELD-SYMBOL(<ls_level_text_part>).

      IF <ls_level_text_part>-no_abbrev_ind = abap_true.

      ELSE.

        IF <ls_level_text_part>-level < iv_level.
          <ls_level_text_part>-text = _abbreviate_text(
            iv_text  = <ls_level_text_part>-text
            iv_level = iv_level ).
          <ls_level_text_part>-level = iv_level.
          RETURN.
        ENDIF.

      ENDIF.

      IF iv_abbrev_right_to_left_ind = abap_true.
        lv_index -= 1.
        IF lv_index = 0.
          EXIT.
        ENDIF.
      ELSE.
        lv_index += 1.
        IF lv_index > lv_count.
          EXIT.
        ENDIF.
      ENDIF.

    ENDDO.

    rv_level_up_ind = abap_true.

  ENDMETHOD.


  METHOD _abbreviate_text.

    CASE iv_level.

      WHEN 1.

        DATA(lv_length) = strlen( iv_text ).
        DO lv_length TIMES.

          DATA(lv_pos_index) = sy-index - 1.

          DATA(lv_char) = iv_text+lv_pos_index(1).
          IF to_upper( lv_char ) CN 'AEIOUY' OR sy-index = 1.
            rv_text = rv_text && lv_char.
          ENDIF.

        ENDDO.

      WHEN 2.

        rv_text = iv_text+0(1).

      WHEN OTHERS.
        ASSERT 1 = 0.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
