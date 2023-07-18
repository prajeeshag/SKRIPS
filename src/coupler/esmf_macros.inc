
#define _ERR_CHK(_file,_line) \
if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=_line, file=_file)) return


#define _ESMF_FIELD_(_FIELD, _OPR) \
    call ESMF_StateGet(esmfState, #_FIELD, esmfField, rc=rc) _nl_ \
    _ERR_CHK(__FILE__,__LINE__) _nl_ \
    call ESMF_FieldGet(esmfField, localDE=0, farrayPtr=farrayPtr, rc=rc) _nl_ \
    _ERR_CHK(__FILE__,__LINE__) _nl_ \
    _OPR  \
    if (associated(farrayPtr)) nullify(farrayPtr) _nl_ \
    if (debugLevel >= 1) then _nl_ \
      write (ofile, "(A,A,I6.6,A)") "ATM_", #_FIELD, iLoop, ".nc" _nl_ \
      call ESMF_FieldWrite(field, trim(ofile), rc=rc) _nl_ \
      _ERR_CHK(__FILE__,__LINE__) _nl_ \
    end if

#define _ATM_GET_OPR(_FIELD) \
    head_grid%_FIELD(:,:) = farrayPtr(:,:) _nl_ \

#define _ATM_PUT_OPR(_FIELD) \
    farrayPtr(:,:) = head_grid%_FIELD(:,:) _nl_ \

#define _ATM_GET(_FIELD) \
  _ESMF_FIELD_(_FIELD, _ATM_GET_OPR(_FIELD))

#define _ATM_PUT(_FIELD) \
  _ESMF_FIELD_(_FIELD, _ATM_PUT_OPR(_FIELD))
