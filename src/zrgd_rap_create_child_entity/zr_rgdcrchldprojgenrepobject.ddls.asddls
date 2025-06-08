@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Generated Repository Object'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@Metadata.allowExtensions: true

define view entity ZR_RgdCrChldProjGenRepObject
  as

  select from zrgd_gen_rep_obj

  association to parent ZR_RgdCreateChildProject as _Project on _Project.ProjectId = $projection.ProjectId

{
  key gen_rep_obj_id  as GenRepObjId,
      project_id      as ProjectId,
      object_type     as ObjectType,
      object_name     as ObjectName,
      
      line_count      as CodeLineCount,

      @Semantics.largeObject: {
        mimeType : 'CodeMimetype',
        fileName : 'CodeFilename',
        //This opens in the browser a new tab.
        contentDispositionPreference: #INLINE
      }
      code_attachment as CodeAttachement,

      @Semantics.mimeType: true
      code_mimetype   as CodeMimetype,

      code_filename   as CodeFilename,

      _Project
}
