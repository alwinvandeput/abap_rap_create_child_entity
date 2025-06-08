@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@ObjectModel.sapObjectNodeType.name: 'ZRGD_CRT_CHILD'

define root view entity ZR_RgdCreateChildProject
  as select from zrgd_crt_child as Project

  composition [0..*] of ZR_RgdCrChldProjGenRepObject as _GeneratedRepositoryObject
  
{
  key project_id                  as ProjectId,
      abap_namespace              as AbapNamespace,
      module_abbreviation         as ModuleAbbreviation,
      parent_is_root_ind          as ParentIsRootInd,
      use_projection_layer_ind    as UseProjectionLayerInd,


      @Consumption.valueHelpDefinition: [
        { entity: {
            name   : 'ZI_RGDDATASOURCE',
            element: 'name' }
        } ]
      bo_root_cds_entity_name     as BoRootCdsEntityName,
      root_rap_alias_name         as RootRapAliasName,
      root_entity_name            as RootEntityName, //NOT USED
      root_has_behavior_def_ind   as RootEnttHasBehaviorDefInd,

      @Consumption.valueHelpDefinition: [
        { entity: {
            name   : 'ZI_RGDDATASOURCE',
            element: 'name' }
        } ]
      bo_parent_cds_entity_name   as BoParentCdsEntityName,
      parent_rap_alias_name       as ParentRapAliasName,
      parent_entity_name          as ParentEntityName, //NOT USED
      parent_has_behavior_def_ind as ParentHasBehaviorDefInd,

      child_db_table_name         as ChildDbTableName,
      child_draft_db_table_name   as ChildDraftDbTableName,
      child_entity_name           as ChildEntityName,
      child_rap_alias_name        as ChildRapAliasName,
      bo_child_cds_entity_name    as BoChildCdsEntityName,
      metadata_extension_name     as MetadataExtensionName,
      @Semantics.user.createdBy: true
      local_created_by            as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      local_created_at            as LocalCreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by       as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at       as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at             as LastChangedAt,

      //Associations
      _GeneratedRepositoryObject

}
