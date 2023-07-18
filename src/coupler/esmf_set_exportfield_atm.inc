
if (associated(farrayPtr)) nullify(farrayPtr) 
call ESMF_StateGet(exportState, "_fieldname", field, rc=rc) 
_ERR_CHK(__FILE,__LINE) 
call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr, rc=rc) 
_ERR_CHK(__FILE,__LINE) 
farrayPtr(:,:) = head_grid%_fieldname(:,:) 
if (associated(farrayPtr)) nullify(farrayPtr)
