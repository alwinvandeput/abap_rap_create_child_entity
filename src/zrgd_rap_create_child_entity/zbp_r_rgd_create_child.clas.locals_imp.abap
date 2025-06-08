CLASS lhc_zr_rgdcreatechildproject DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.


    DATA gv_temp_tky TYPE STRUCTURE FOR PERMISSIONS KEY zr_rgdcreatechildproject.
    DATA gt_temp_reported TYPE TABLE FOR REPORTED zr_rgdcreatechildproject.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING
      REQUEST requested_authorizations FOR Project
      RESULT result.

    METHODS FillByExample FOR MODIFY
      IMPORTING keys FOR ACTION Project~FillByExample RESULT result.

    METHODS FillFieldsOnCreate FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Project~FillFieldsOnCreate.

    METHODS OverwriteChildNames FOR MODIFY
      IMPORTING keys FOR ACTION Project~OverwriteChildNames RESULT result.

    METHODS SetRootFields FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Project~SetRootFields.

    METHODS UpdateEmptyDerivedFields FOR MODIFY
      IMPORTING keys FOR ACTION Project~UpdateEmptyDerivedFields RESULT result.

    METHODS ValidateProject FOR VALIDATE ON SAVE
      IMPORTING keys FOR Project~ValidateProject.
    METHODS ValidateAbapNamespace FOR VALIDATE ON SAVE
      IMPORTING keys FOR Project~ValidateAbapNamespace.

    METHODS GenerateRepositoryObjects FOR MODIFY
      IMPORTING keys FOR ACTION Project~GenerateRepositoryObjects.

    TYPES ts_project TYPE STRUCTURE FOR READ RESULT zr_rgdcreatechildproject.
    TYPES: ts_helper_type   TYPE ts_project,
           ts_helper_type_1 TYPE ts_project.

    METHODS _update_child_entity
      IMPORTING iv_Overwrite_Child_Names_Ind TYPE abap_boolean DEFAULT abap_false
      CHANGING
                cs_project                   TYPE ts_helper_type_1.

    METHODS _update_parent_entity
      CHANGING
        cs_project TYPE ts_helper_type.

    METHODS _update_derived_fields
      CHANGING cs_project TYPE ts_project.

    METHODS _update_root_entity
      CHANGING
        cs_project TYPE lhc_zr_rgdcreatechildproject=>ts_project.

    METHODS _check_required_field
      IMPORTING
        label TYPE string
        val   TYPE any.

ENDCLASS.

CLASS lhc_zr_rgdcreatechildproject IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.


  METHOD FillByExample.

    READ ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
      ENTITY Project
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_projects)
      FAILED DATA(lt_failed).

    LOOP AT lt_projects
      ASSIGNING FIELD-SYMBOL(<ls_project>).

      "Basic Settings
      <ls_project>-abapnamespace            = 'Z'.
      <ls_project>-ModuleAbbreviation       = 'SD'.
      <ls_project>-ParentIsRootInd          = abap_true.
      "<ls_project>-UseBasicLayerInd           = abap_false.
      "<ls_project>-UseUiProjectionLayerInd    = abap_false.
      "<ls_project>-UseApiProjectionLayerInd   = abap_false.

*      <ls_project>-RootRapAliasName         = 'SalesOrder'.           "Could be read from CDS or Beh. def.
      <ls_project>-BoRootCdsEntityName      = 'ZR_SdSalesOrderTP'.    "Rename Root BoCdsEntityName

      <ls_project>-ParentRapAliasName       = 'SalesOrderItem'.        "Could be read from CDS or Beh. def.
      <ls_project>-BoParentCdsEntityName    = 'ZR_SdSalesOrderItemTP'.

      <ls_project>-ChildDbTableName         = 'ZSD_SCHED_LINE'.
      <ls_project>-ChildEntityName          = 'Schedule Line'.
      <ls_project>-ChildRapAliasName        = 'ScheduleLine'.         "Derived
      <ls_project>-BoChildCdsEntityName     = 'ZR_SdScheduleLineTP'.  "Derived
      <ls_project>-ChildDraftDbTableName    = 'ZSD_SCHED_LINED'.      "Derived

      <ls_project>-MetadataExtensionName    = 'ZC_SdScheduleLineTP'.  "Derived -Rename to ChildUiMetadataExtension


*NOT NEEDED
*      <ls_project>-RootEntityName           = ''.
*      <ls_project>-ParentEntityName         = ''.

    ENDLOOP.

    MODIFY ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
      ENTITY Project
      UPDATE
        FIELDS (
          abapnamespace
          ModuleAbbreviation
          ParentIsRootInd

          RootRapAliasName
          BoRootCdsEntityName

          BoParentCdsEntityName
          ParentRapAliasName

          ChildDbTableName
          ChildDraftDbTableName
          ChildEntityName
          ChildRapAliasName
          BoChildCdsEntityName
          MetadataExtensionName

        )
        WITH VALUE #(
          FOR ls_project IN lt_projects (
            %is_draft   = ls_project-%is_draft
            %data       = CORRESPONDING #( ls_project )
            %control = VALUE #(
              abapnamespace         = if_abap_behv=>mk-on
              ModuleAbbreviation    = if_abap_behv=>mk-on
              ParentIsRootInd       = if_abap_behv=>mk-on

              RootRapAliasName      = if_abap_behv=>mk-on
              BoRootCdsEntityName   = if_abap_behv=>mk-on

              BoParentCdsEntityName = if_abap_behv=>mk-on
              ParentRapAliasName    = if_abap_behv=>mk-on

              ChildDbTableName      = if_abap_behv=>mk-on
              ChildDraftDbTableName = if_abap_behv=>mk-on
              ChildEntityName       = if_abap_behv=>mk-on
              ChildRapAliasName     = if_abap_behv=>mk-on
              BoChildCdsEntityName  = if_abap_behv=>mk-on
              MetadataExtensionName = if_abap_behv=>mk-on

              ) ) )
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_modify_reported).

    LOOP AT lt_projects
      ASSIGNING <ls_project>.

      APPEND INITIAL LINE TO result
        ASSIGNING FIELD-SYMBOL(<ls_result>).

      <ls_result>-%tky = <ls_project>-%tky.
      <ls_result>-%param = CORRESPONDING #( <ls_project> ).
    ENDLOOP.

  ENDMETHOD.

  METHOD FillFieldsOnCreate.

    MODIFY ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
      ENTITY project
      UPDATE
        FIELDS (
          AbapNamespace
          ParentIsRootInd )
        WITH VALUE #(
          FOR ls_key IN keys (
            %tky          = ls_key-%tky
            AbapNamespace = 'Z'
            ParentIsRootInd = abap_true
            %control = VALUE #(
              AbapNamespace   = if_abap_behv=>mk-on
              ParentIsRootInd = if_abap_behv=>mk-on ) ) )
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_modify_reported).

  ENDMETHOD.


  METHOD OverwriteChildNames.

    READ ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
         ENTITY Project
         ALL FIELDS
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_projects)
         " TODO: variable is assigned but never used (ABAP cleaner)
         FAILED DATA(lt_failed).

    LOOP AT lt_projects
       ASSIGNING FIELD-SYMBOL(<ls_project>).

      _update_child_entity(
        EXPORTING iv_Overwrite_Child_Names_Ind = abap_true
        CHANGING
          cs_project = <ls_project>
      ).

    ENDLOOP.

    MODIFY ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
           ENTITY Project
           UPDATE
           FIELDS (
             ChildDraftDbTableName
             ChildEntityName
             ChildRapAliasName
             BoChildCdsEntityName
             MetadataExtensionName
           )
           WITH VALUE #( FOR ls_project IN lt_projects
                         ( %is_draft  = ls_project-%is_draft

                           %data = VALUE #(
                             ProjectId                 = ls_project-ProjectId
                             ChildDraftDbTableName     = ls_project-ChildDraftDbTableName
                             ChildEntityName           = ls_project-ChildEntityName
                             ChildRapAliasName         = ls_project-ChildRapAliasName
                             BoChildCdsEntityName      = ls_project-BoChildCdsEntityName
                             MetadataExtensionName     = ls_project-MetadataExtensionName
                           )

                           %control = VALUE #(
                               ChildDraftDbTableName     = if_abap_behv=>mk-on
                               ChildEntityName           = if_abap_behv=>mk-on
                               ChildRapAliasName         = if_abap_behv=>mk-on
                               BoChildCdsEntityName      = if_abap_behv=>mk-on
                               MetadataExtensionName     = if_abap_behv=>mk-on
                          ) ) )
           " TODO: variable is assigned but never used (ABAP cleaner)
           FAILED DATA(ls_failed)
           " TODO: variable is assigned but never used (ABAP cleaner)
           REPORTED DATA(ls_modify_reported).

    "Set result
    LOOP AT lt_projects
         ASSIGNING <ls_project>.
      APPEND INITIAL LINE TO result
             ASSIGNING FIELD-SYMBOL(<ls_result>).
      <ls_result>-%tky   = <ls_project>-%tky.
      <ls_result>-%param = CORRESPONDING #( <ls_project> ).
    ENDLOOP.

  ENDMETHOD.

  METHOD SetRootFields.


  ENDMETHOD.


  METHOD UpdateEmptyDerivedFields.

    READ ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
         ENTITY Project
         ALL FIELDS
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_projects)
         " TODO: variable is assigned but never used (ABAP cleaner)
         FAILED DATA(lt_failed).

    LOOP AT lt_projects
         ASSIGNING FIELD-SYMBOL(<ls_project>).

      _update_derived_fields( CHANGING cs_project = <ls_project> ).

    ENDLOOP.

    MODIFY ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
           ENTITY Project
           UPDATE
           FIELDS (
             RootRapAliasName
             RootEnttHasBehaviorDefInd

             ParentRapAliasName
             ParentHasBehaviorDefInd

             ChildDraftDbTableName
             ChildEntityName
             ChildRapAliasName
             BoChildCdsEntityName
             MetadataExtensionName
           )
           WITH VALUE #( FOR ls_project IN lt_projects
                         ( %is_draft  = ls_project-%is_draft

                           %data = VALUE #(
                             ProjectId                 = ls_project-ProjectId
                             RootRapAliasName          = ls_project-RootRapAliasName
                             RootEnttHasBehaviorDefInd = ls_project-RootEnttHasBehaviorDefInd
                             ParentRapAliasName        = ls_project-ParentRapAliasName
                             ParentHasBehaviorDefInd   = ls_project-ParentHasBehaviorDefInd
                             ChildDraftDbTableName     = ls_project-ChildDraftDbTableName
                             ChildEntityName           = ls_project-ChildEntityName
                             ChildRapAliasName         = ls_project-ChildRapAliasName
                             BoChildCdsEntityName      = ls_project-BoChildCdsEntityName
                             MetadataExtensionName     = ls_project-MetadataExtensionName
                           )

                           %control = VALUE #(
                               RootRapAliasName          = if_abap_behv=>mk-on
                               RootEnttHasBehaviorDefInd = if_abap_behv=>mk-on
                               ParentRapAliasName        = if_abap_behv=>mk-on
                               ParentHasBehaviorDefInd   = if_abap_behv=>mk-on
                               ChildDraftDbTableName     = if_abap_behv=>mk-on
                               ChildEntityName           = if_abap_behv=>mk-on
                               ChildRapAliasName         = if_abap_behv=>mk-on
                               BoChildCdsEntityName      = if_abap_behv=>mk-on
                               MetadataExtensionName     = if_abap_behv=>mk-on
                          ) ) )
           " TODO: variable is assigned but never used (ABAP cleaner)
           FAILED DATA(ls_failed)
           " TODO: variable is assigned but never used (ABAP cleaner)
           REPORTED DATA(ls_modify_reported).

    "Set result
    LOOP AT lt_projects
         ASSIGNING <ls_project>.
      APPEND INITIAL LINE TO result
             ASSIGNING FIELD-SYMBOL(<ls_result>).
      <ls_result>-%tky   = <ls_project>-%tky.
      <ls_result>-%param = CORRESPONDING #( <ls_project> ).
    ENDLOOP.

  ENDMETHOD.

  METHOD _update_derived_fields.

    _update_root_entity(
      CHANGING
        cs_project = cs_project ).

    _update_parent_entity(
      CHANGING
        cs_project = cs_project ).

    _update_child_entity(
      CHANGING
        cs_project = cs_project ).

  ENDMETHOD.


  METHOD _update_root_entity.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Behavior Definition
    IF cs_project-BoRootCdsEntityName IS INITIAL.
      cs_project-RootEnttHasBehaviorDefInd = abap_false.

    ELSE.
      DATA(lo_behavior) = xco_cp_abap_repository=>object->bdef->for( cs_project-BoRootCdsEntityName ).
      IF lo_behavior->exists( ) = abap_true.
        DATA(lo_beh_content) = lo_behavior->content( ).
        DATA(lv_short_description) = lo_beh_content->get_short_description( ).
        cs_project-RootEnttHasBehaviorDefInd = abap_true.
      ELSE.
        cs_project-RootEnttHasBehaviorDefInd = abap_false.
      ENDIF.

    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " CDS View
    IF     cs_project-BoRootCdsEntityName IS NOT INITIAL
       AND cs_project-RootRapAliasName    IS INITIAL.
      DATA(lo_data_definition) = xco_cp_abap_repository=>object->ddls->for(
                                     to_upper( cs_project-BoRootCdsEntityName ) ).
      DATA(lo_view_entity) = lo_data_definition->view_entity( ).
      DATA(lo_content) = lo_view_entity->content( ).
      DATA(ls_data_source) = lo_content->get_data_source( ).

      IF ls_data_source-alias IS NOT INITIAL.
        cs_project-RootRapAliasName = ls_data_source-alias.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD _update_parent_entity.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Behavior Definition
    IF cs_project-BoParentCdsEntityName IS INITIAL.
      cs_project-ParentHasBehaviorDefInd = abap_false.

    ELSE.
      DATA(lo_behavior) = xco_cp_abap_repository=>object->bdef->for( cs_project-BoParentCdsEntityName ).
      IF lo_behavior->exists( ) = abap_true.
        "DATA(lo_beh_content) = lo_behavior->content( ).
        cs_project-ParentHasBehaviorDefInd = abap_true.
      ELSE.
        cs_project-ParentHasBehaviorDefInd = abap_false.
      ENDIF.

    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " CDS View
    IF     cs_project-BoParentCdsEntityName IS NOT INITIAL
       AND cs_project-ParentEntityName    IS INITIAL.
      DATA(lo_data_definition) = xco_cp_abap_repository=>object->ddls->for(
                                     to_upper( cs_project-BoParentCdsEntityName ) ).
      DATA(lo_view_entity) = lo_data_definition->view_entity( ).
      DATA(lo_content) = lo_view_entity->content( ).
      DATA(ls_data_source) = lo_content->get_data_source( ).

      IF ls_data_source-alias IS NOT INITIAL.
        cs_project-ParentRapAliasName = ls_data_source-alias.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD _update_child_entity.

    " Child Draft Table
    IF    cs_project-ChildDraftDbTableName IS INITIAL
       OR iv_Overwrite_Child_Names_Ind      = abap_true.

      IF cs_project-ChildDbTableName IS NOT INITIAL.

        DATA(lo_name_generator) = NEW zrgd_abbreviated_name_gen( ).

        DATA(lv_name) = lo_name_generator->get_name(
          iv_abbrev_right_to_left_ind = abap_true
          iv_max_length    = 16
          it_text_parts    = VALUE #(
            ( text = cs_project-ChildDbTableName
              split_char = |_|
              split_text = |_|
              no_abbrev_part_till_index = 1 )
            ( text = 'D' no_abbrev_ind = abap_true ) ) ).

        cs_project-ChildDraftDbTableName = lv_name.

      ENDIF.

    ENDIF.

    " Child Entity Name
    IF cs_project-ChildEntityName IS INITIAL.

      DATA(lo_data_definition) = xco_cp_abap_repository=>object->tabl->for(
                                     to_upper( cs_project-ChildDbTableName ) ).
      IF lo_data_definition->exists( ) = abap_true.

        IF lo_data_definition->is_database_table( ) = abap_true.

          DATA(lo_db_table) = lo_data_definition->get_database_table( ).

          DATA(lo_content) = lo_db_table->content( ).
          " TODO: variable is assigned but never used (ABAP cleaner)
          DATA(lo_header) = lo_content->get( ).
          DATA(lv_short_descr) = lo_content->get_short_description( ).

          cs_project-ChildEntityName = lv_short_descr.

        ENDIF.

      ENDIF.

    ENDIF.

    " Child Alias Name
    IF    cs_project-ChildRapAliasName IS INITIAL
       OR iv_Overwrite_Child_Names_Ind  = abap_true.

      IF cs_project-ChildEntityName IS NOT INITIAL.

        lo_name_generator = NEW zrgd_abbreviated_name_gen( ).

        lv_name = lo_name_generator->get_name(
          iv_max_length    = 30
          it_text_parts    = VALUE #(
            ( text = cs_project-ChildEntityName split_by_space_ind = abap_true ) ) ).

        cs_project-ChildRapAliasName = lv_name.

      ENDIF.

    ENDIF.

    " Child Entity CDS Name
    IF    cs_project-BoChildCdsEntityName IS INITIAL
       OR iv_Overwrite_Child_Names_Ind     = abap_true.

      IF cs_project-ChildEntityName IS NOT INITIAL.

        lo_name_generator = NEW zrgd_abbreviated_name_gen( ).

        lv_name = lo_name_generator->get_name(
          iv_max_length    = 30
          it_text_parts    = VALUE #(
            ( text = cs_project-AbapNamespace no_abbrev_ind = abap_true )
            ( text = 'R_' no_abbrev_ind = abap_true )
            ( text = cs_project-ModuleAbbreviation
              no_abbrev_ind = abap_true
              Only_First_Char_Upper_ind = abap_true )
            ( text = cs_project-ChildEntityName split_by_space_ind = abap_true )
            ( text = 'TP'  no_abbrev_ind = abap_true ) ) ).

        cs_project-BoChildCdsEntityName = lv_name.

      ENDIF.

    ENDIF.

    " Child Metadata Extension Name
    IF    cs_project-MetadataExtensionName IS INITIAL
       OR iv_Overwrite_Child_Names_Ind      = abap_true.

      IF cs_project-ChildEntityName IS NOT INITIAL.

        lo_name_generator = NEW zrgd_abbreviated_name_gen( ).

        lv_name = lo_name_generator->get_name(
          iv_max_length    = 30
          it_text_parts    = VALUE #(
            ( text = cs_project-AbapNamespace no_abbrev_ind = abap_true )
            ( text = 'C_' no_abbrev_ind = abap_true )
            ( text = cs_project-ModuleAbbreviation
              no_abbrev_ind = abap_true
              Only_First_Char_Upper_ind = abap_true )
            ( text = cs_project-ChildEntityName split_by_space_ind = abap_true )
            ( text = 'TP'  no_abbrev_ind = abap_true ) ) ).

        cs_project-MetadataExtensionName = lv_name.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD ValidateProject.

    READ ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
         ENTITY Project
         ALL FIELDS
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_projects)
         " TODO: variable is assigned but never used (ABAP cleaner)
         FAILED DATA(lt_failed).

    LOOP AT lt_projects
         ASSIGNING FIELD-SYMBOL(<ls_project>).

      CLEAR gt_temp_reported.
      gv_temp_tky-%tky = <ls_project>-%tky.

      IF <ls_project>-ABAPNamespace IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-abapnamespace = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |ABAP Namespace is not filled.| ) )
          TO reported-project.
      ENDIF.

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Root Entity

      IF <ls_project>-ParentIsRootInd = abap_true.

        IF <ls_project>-BoRootCdsEntityName IS NOT INITIAL.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-BoRootCdsEntityName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                "TODO: text is too long.
                text     = |Parent is Root, so Root CDS Entity Name must be empty.| ) )
            TO reported-project.
        ENDIF.

        IF <ls_project>-RootRapAliasName IS NOT INITIAL.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-RootRapAliasName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                "TODO: text is too long.
                text     = |Parent is Root, so Root RAP Alias Name must be empty.| ) )
            TO reported-project.
        ENDIF.

      ELSE.

        IF <ls_project>-BoRootCdsEntityName IS INITIAL.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-BoRootCdsEntityName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                text     = |Parent is not Root. Root BO CDS Entity is not filled.| ) )
            TO reported-project.
        ENDIF.

        IF <ls_project>-RootRapAliasName IS INITIAL.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-RootRapAliasName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                text     = |Parent is not Root. Root Alias Name is not filled.| ) )
            TO reported-project.
        ELSE.
          IF <ls_project>-RootRapAliasName CS | |.
            APPEND VALUE #(
                %tky = gv_temp_tky-%tky
                %element-RootRapAliasName = if_abap_behv=>mk-on
                %msg = new_message_with_text(
                  severity = if_abap_behv_message=>severity-error
                  text     = |Root Alias Name must not contain spaces.| ) )
              TO reported-project.
          ENDIF.
        ENDIF.

      ENDIF.

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Parent Entity
      IF <ls_project>-ParentRapAliasName IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-ParentRapAliasName = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |Parent Alias Name is not filled.| ) )
          TO reported-project.
      ELSE.
        IF <ls_project>-ParentRapAliasName CS | |.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-ParentRapAliasName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                text     = |Parent Alias Name must not contain spaces.| ) )
            TO reported-project.
        ENDIF.
      ENDIF.

      IF <ls_project>-BoParentCdsEntityName IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-BoParentCdsEntityName = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |Parent CDS Name is not filled.| ) )
          TO reported-project.
      ENDIF.

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Child Entity
      IF <ls_project>-ChildEntityName IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-ChildEntityName = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |Child Entity Name is not filled.| ) )
          TO reported-project.
      ENDIF.

      IF <ls_project>-ChildRapAliasName IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-ChildRapAliasName = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |Child Alias Name is not filled.| ) )
          TO reported-project.
      ELSE.
        IF <ls_project>-ChildRapAliasName CS | |.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-ChildRapAliasName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                text     = |Child Alias Name must not contain spaces.| ) )
            TO reported-project.
        ENDIF.
      ENDIF.

      IF <ls_project>-ChildDbTableName IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-ChildDbTableName = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |Child DB Table Name is not filled.| ) )
          TO reported-project.
      ELSE.
        IF <ls_project>-ChildDbTableName CS | |.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-ChildDbTableName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                text     = |DB Table Name must not contain spaces.| ) )
            TO reported-project.
        ENDIF.
      ENDIF.

      IF <ls_project>-ChildDraftDbTableName IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-ChildDraftDbTableName = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |Child Draft DB Table Name is not filled.| ) )
          TO reported-project.
      ELSE.
        IF <ls_project>-ChildDraftDbTableName CS | |.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-ChildDraftDbTableName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                text     = |Draft DB Table Name must not contain spaces.| ) )
            TO reported-project.
        ENDIF.
      ENDIF.

      IF <ls_project>-BoChildCdsEntityName IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-BoChildCdsEntityName = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |Child CDS Name is not filled.| ) )
          TO reported-project.
      ELSE.
        IF <ls_project>-BoChildCdsEntityName CS | |.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-BoChildCdsEntityName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                text     = |Child CDS Name must not contain spaces.| ) )
            TO reported-project.
        ENDIF.
      ENDIF.

      IF <ls_project>-MetadataExtensionName IS INITIAL.
        APPEND VALUE #(
            %tky = gv_temp_tky-%tky
            %element-MetadataExtensionName = if_abap_behv=>mk-on
            %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = |Child Metadata Ext. Name is not filled.| ) )
          TO reported-project.
      ELSE.
        IF <ls_project>-MetadataExtensionName CS | |.
          APPEND VALUE #(
              %tky = gv_temp_tky-%tky
              %element-MetadataExtensionName = if_abap_behv=>mk-on
              %msg = new_message_with_text(
                severity = if_abap_behv_message=>severity-error
                "TODO: text is too long
                text     = |Child Metadata Extension Name must not contain spaces.| ) )
            TO reported-project.
        ENDIF.
      ENDIF.

      "Add Failed and Reported
      IF reported-project IS NOT INITIAL.
        APPEND VALUE #( %tky = <ls_project>-%tky ) TO failed-project.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD _check_required_field.

    IF val IS INITIAL.
      APPEND
        VALUE #(
          %tky = gv_temp_tky-%tky
          %element-abapnamespace = if_abap_behv=>mk-on
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text     = |{ label } is not filled.| ) )
        TO gt_temp_reported.
    ENDIF.

  ENDMETHOD.

  METHOD ValidateAbapNamespace.

*    READ ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
*         ENTITY Project
*         ALL FIELDS
*         WITH CORRESPONDING #( keys )
*         RESULT DATA(lt_projects)
*         " TODO: variable is assigned but never used (ABAP cleaner)
*         FAILED DATA(lt_failed).
*
*    LOOP AT lt_projects
*         ASSIGNING FIELD-SYMBOL(<ls_project>).
*
*      IF <ls_project>-ABAPNamespace IS INITIAL.
*
*        APPEND VALUE #( %tky = <ls_project>-%tky ) TO failed-project.
*
*        APPEND
*          VALUE #(
*            %tky = gv_temp_tky-%tky
*            %msg = new_message_with_text(
*              severity = if_abap_behv_message=>severity-error
*              text     = |ABAP Namespace is not filled.| )
*            %element-abapnamespace = if_abap_behv=>mk-on
*          )
*          TO reported-project.
*
*      ENDIF.

*    ENDLOOP.

  ENDMETHOD.

  METHOD GenerateRepositoryObjects.

    READ ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
         ENTITY Project
         ALL FIELDS
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_projects)
         " TODO: variable is assigned but never used (ABAP cleaner)
         FAILED DATA(lt_failed).

    LOOP AT lt_projects
         ASSIGNING FIELD-SYMBOL(<ls_project>).

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Delete
      SELECT *
        FROM zr_rgdcrchldprojgenrepobject
        WHERE ProjectId = @<ls_project>-ProjectId
        INTO TABLE @DATA(lt_existing_objects).

      IF lt_existing_objects IS NOT INITIAL.

        "Delete from Behavior Definition:
        MODIFY ENTITIES OF ZR_RgdCreateChildProject IN LOCAL MODE

          "Child Entities:
          ENTITY GeneratedRepositoryObject
            DELETE
              FROM VALUE #(
                FOR ls_object  IN lt_existing_objects INDEX INTO lv_index
                (
                  %tky-GenRepObjId = ls_object-GenRepObjId
                )
              )

        MAPPED DATA(delete_mapped)
        REPORTED DATA(delete_reported)
        FAILED DATA(delete_failed).
      ENDIF.

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Insert
      DATA(cr_lf) = cl_abap_char_utilities=>cr_lf.

      DATA lt_source_code_lines TYPE string_table.

      APPEND |CLASS zzap_test DEFINITION.| TO lt_source_code_lines.
      DO 10000 TIMES.
        APPEND  |  Line { sy-index }.| TO lt_source_code_lines.
      ENDDO.
      APPEND |ENDCLASS.|  TO lt_source_code_lines.

      DATA(lv_line_count) = lines( lt_source_code_lines ).

      DATA lv_source TYPE string.
      LOOP AT lt_source_code_lines
        ASSIGNING FIELD-SYMBOL(<lv_line>).
        IF sy-tabix = 1.
          lv_source = <lv_line>.
        ELSE.
          lv_source = lv_source && cr_lf && <lv_line>.
        ENDIF.
      ENDLOOP.

*      DATA(lv_base64_content) = cl_web_http_utility=>encode_x_base64( conv xstring( lv_source ) ).

      DATA ls_object2 TYPE zr_rgdcrchldprojgenrepobject.
      ls_object2 = VALUE #(
          "GenRepObjId,
          "ProjectId,
          ObjectType      = 'TABL'
          ObjectName      = 'ZZAP_CLASS'
          CodeLineCount   = lv_line_count
          CodeAttachement = CONV xstring( lv_source )
          CodeMimetype    = 'text/plain'
          CodeFilename    = |Step 000001 CREATE CLAS_ZZAP_CLASS 2.txt|
        ).

      DATA(xstring) = cl_abap_conv_codepage=>create_out(
        codepage = `UTF-8`
        )->convert( source = lv_source ).

      ls_object2-CodeAttachement = xstring.

      DATA lt_objects TYPE STANDARD TABLE OF zr_rgdcrchldprojgenrepobject.
      APPEND ls_object2 TO lt_objects.

      MODIFY ENTITIES OF zr_rgdcreatechildproject IN LOCAL MODE
        ENTITY Project
          CREATE BY \_GeneratedRepositoryObject
            FROM VALUE #(
              "Run
              (
                %tky = <ls_project>-%tky
                %target = VALUE #(
                  FOR ls_object  IN lt_objects INDEX INTO lv_index
                  (
                     %cid                    = |cid{ lv_index }|
                     ObjectType              = ls_object-ObjectType
                     %control-ObjectType     = if_abap_behv=>mk-on
                     ObjectName              = ls_object-ObjectName
                     %control-ObjectName     = if_abap_behv=>mk-on
                     CodeLineCount           = ls_object-CodeLineCount
                     %control-CodeLineCount  = if_abap_behv=>mk-on

                     CodeAttachement          = ls_object-CodeAttachement
                     %control-CodeAttachement = if_abap_behv=>mk-on
                     CodeMimetype             = ls_object-CodeMimetype
                     %control-CodeMimetype    = if_abap_behv=>mk-on
                     CodeFilename             = ls_object-CodeFilename
                     %control-CodeFilename    = if_abap_behv=>mk-on
                  )
                )
              )
            )
      MAPPED DATA(create_mapped)
      REPORTED DATA(create_reported)
      FAILED DATA(create_failed).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lhc_generatedrepositoryobject DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR GeneratedRepositoryObject RESULT result.

    METHODS GetCode FOR MODIFY
      IMPORTING keys FOR ACTION GeneratedRepositoryObject~GetCode.

ENDCLASS.

CLASS lhc_generatedrepositoryobject IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD GetCode.
  ENDMETHOD.

ENDCLASS.
