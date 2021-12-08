program main
use parameters
implicit none
integer(4)::narg
character(512)::parafile

narg=iargc()
!!!!! write(*,*) 'number of parameters',narg
if (narg.lt.1) then
  write(*,*) 'missing parameter file.  Program end.'
  stop
endif

call getarg(1,parafile)
call load_para(parafile)

endprogram main
