module parameters
implicit none

integer(4),parameter::npara=47  ! number of total parameters
integer(4),parameter::nreqpara=4  ! number of mandatory parameters
integer(kind(npara))::keyid
character(512)::reqpara(npara)  ! store parameter names (key)
logical(1)::paraflag(npara)   ! flags
character(512)::defvalue(npara)  ! default values for optional parameters
character(512)::message(npara)  ! output messages for all the parameters

! mandatory para
real(4)::box     ! box size
integer(4)::LL(3)   ! mesh size
character(512)::infg  ! input galaxy filename
integer(4)::ngal  ! number of galaxies

! optional para
character(512)::outname  ! output filename

logical(4)::flag_gcata !
integer(4)::gxyz(3)
integer(4)::nghead
integer(4)::painter
logical(4)::wincorr
logical(4)::interlace

logical(4)::flag_r
character(512)::infr
logical(4)::flag_rcata
integer(4)::rxyz(3)
integer(4)::nrhead
integer(4)::nran

logical(4)::sncorr
logical(4)::usedefaultkminkmax
real(4)::kmin    ! input kmin kmax if usedefault=false
real(4)::kmax    ! input kmin kmax if usedefault=false
logical(4)::logbin
integer(4)::kbin   ! kbin and dk can not be zero simuteneously
real(4)::dk         ! kbin and dk can not be zero simuteneously
logical(4)::usecenter

logical(4)::flag_pk2d
character(512)::outname2
real(4)::umin
real(4)::umax
integer(4)::ubin

logical(4)::second
logical(4)::flag_gcata2
character(512)::infg2
integer(4)::gxyz2(3)
integer(4)::nghead2
integer(4)::ngal2

logical(4)::flag_r2
logical(4)::flag_rcata2  ! input random is a catalogue, otherwise, a field
character(512)::infr2    ! random catalgue name
integer(4)::rxyz2(3)       ! columns for x y z
integer(4)::nrhead2              ! number of lines for header
integer(4)::nran2        ! number of lines for random

logical(4)::interlace2
integer(4)::painter2   ! 2 for cic
logical(4)::wincorr2   ! whether deconvolve window for field2, for auto, field2=field1

logical(4)::othernpeff  ! set effective particle number to correct shot noise
real(4)::npeffinput

contains


!!! set the key for all the parameters
!!! set the default value string for all the optional parameters
subroutine initpara
implicit none
paraflag=.true.
! mandatory para
reqpara(1)='box'
reqpara(2)='LL'
reqpara(3)='infg'
reqpara(4)='ngal'

! optional para
reqpara(5)='outname'
defvalue(5)='pkl.txt'

reqpara(6)='flag_gcata'
defvalue(6)='T'

reqpara(7)='gxyz'
defvalue(7)='1 2 3'

reqpara(8)='nghead'
defvalue(8)='0'

reqpara(9)='painter'
defvalue(9)='2'

reqpara(10)='wincorr'
defvalue(10)='F'

reqpara(11)='interlace'
defvalue(11)='F'

reqpara(12)='flag_r'
defvalue(12)='F'

reqpara(13)='infr'
defvalue(13)=' '

reqpara(14)='flag_rcata'
defvalue(14)='F'

reqpara(15)='rxyz'
defvalue(15)='1 2 3'

reqpara(16)='nrhead'
defvalue(16)='0'

reqpara(17)='nran'
defvalue(17)='0'

reqpara(18)='sncorr'
defvalue(18)='F'

reqpara(19)='usedefaulkminkmax'
defvalue(19)='T'

reqpara(20)='kmin'
defvalue(20)='0.'

reqpara(21)='kmax'
defvalue(21)='0.'

reqpara(22)='logbin'
defvalue(22)='T'

reqpara(23)='kbin'
defvalue(23)='20'

reqpara(24)='dk'
defvalue(24)='0.'

reqpara(25)='usecenter'
defvalue(25)='T'

reqpara(26)='flag_pk2d'
defvalue(26)='F'

reqpara(27)='outname2'
defvalue(27)='pk2d.txt'

reqpara(28)='umin'
defvalue(28)='0.'

reqpara(29)='umax'
defvalue(29)='1.'

reqpara(30)='ubin'
defvalue(30)='5'

reqpara(31)='second'
defvalue(31)='F'

reqpara(32)='flag_gcata2'
defvalue(32)='T'

reqpara(33)='infg2'
defvalue(33)=' '

reqpara(34)='gxyz2'
defvalue(34)='1 2 3'

reqpara(35)='nghead2'
defvalue(35)='0'

reqpara(36)='ngal2'
defvalue(36)='0'

reqpara(37)='flag_r2'
defvalue(37)='F'

reqpara(38)='flag_rcata2'
defvalue(38)='T'

reqpara(39)='infr2'
defvalue(39)=' '

reqpara(40)='rxyz2'
defvalue(40)='1 2 3'

reqpara(41)='nrhead2'
defvalue(41)='0'

reqpara(42)='nran2'
defvalue(42)='0'

reqpara(43)='interlace2'
defvalue(43)='F'

reqpara(44)='painter2'
defvalue(44)='2'

reqpara(45)='wincorr2'
defvalue(45)='F'

reqpara(46)='othernpeff'
defvalue(46)='F'

reqpara(47)='npeffinput'
defvalue(47)='0'
endsubroutine initpara

!!! according to the keyid
!!! read in the string and set the variables according to the varaible kind
!!! rewrite the variables into specified format and combine into output variable message 
subroutine setpara(keyid,content)
implicit none
character(*)::content
character(512)::content2
integer(4)::keyid
select case (keyid)
  case(1)
    read(content,*) box
    write(content2,'(f)') box
  case(2)
    read(content,*) LL(1:3)
    write(content2,'(3i5)') LL(1:3)
  case(3)
    infg=trim(content)
    content2=trim(infg)
  case(4)
    read(content,*) ngal
    write(content2,'(i10)') ngal
  case(5)
    outname=trim(content)
    content2=trim(outname)
  case(6)
    read(content,*) flag_gcata
    write(content2,*) flag_gcata
  case(7)
    read(content,*) gxyz(1:3)
    write(content2,'(3i2)') gxyz(1:3)
  case(8)
    read(content,*) nghead
    write(content2,'(i10)') nghead
  case(9)
    read(content,*) painter
    write(content2,'(i3)') painter
  case(10)
    read(content,*) wincorr
    write(content2,*) wincorr
  case(11)
    read(content,*) interlace
    write(content2,*) interlace
  case(12)
    read(content,*) flag_r
    write(content2,*) flag_r
  case(13)
    infr=trim(content)
    content2=trim(infr)
  case(14)
    read(content,*) flag_rcata
    write(content2,*) flag_rcata
  case(15)
    read(content,*) rxyz(1:3)
    write(content2,'(3i2)') rxyz(1:3)
  case(16)
    read(content,*) nrhead
    write(content2,'(i10)') nrhead
  case(17)
    read(content,*) nran
    write(content2,'(i10)') nran
  case(18)
    read(content,*) sncorr
    write(content2,*) sncorr
  case(19)
    read(content,*) usedefaultkminkmax
    write(content2,*) usedefaultkminkmax
  case(20)
    read(content,*) kmin
    write(content2,'(f)') kmin
  case(21)
    read(content,*) kmax
    write(content2,'(f)') kmax
  case(22)
    read(content,*) logbin
    write(content2,*) logbin
  case(23)
    read(content,*) kbin
    write(content2,'(i5)') kbin
  case(24)
    read(content,*) dk
    write(content2,'(f)') dk
  case(25)
    read(content,*) usecenter
    write(content2,*) usecenter
  case(26)
    read(content,*) flag_pk2d
    write(content2,*) flag_pk2d
  case(27)
    outname2=trim(content)
    content2=trim(outname2)
  case(28)
    read(content,*) umin
    write(content2,'(f)') umin
  case(29)
    read(content,*) umax
    write(content2,'(f)') umax
  case(30)
    read(content,*) ubin
    write(content2,'(i4)') ubin
  case(31)
    read(content,*) second
    write(content2,*) second
  case(32)
    read(content,*) flag_gcata2
    write(content2,*) flag_gcata2
  case(33)
    infg2=trim(content)
    content2=trim(infg2)
  case(34)
    read(content,*) gxyz2(1:3)
    write(content2,'(3i2)') gxyz2(1:3)
  case(35)
    read(content,*) nghead2
    write(content2,'(i10)') nghead2
  case(36)
    read(content,*) ngal2
    write(content2,'(i10)') ngal2
  case(37)
    read(content,*) flag_r2
    write(content2,*) flag_r2
  case(38)
    read(content,*) flag_rcata2
    write(content2,*) flag_rcata2
  case(39)
    infr2=trim(content)
    content2=trim(infr2)
  case(40)
    read(content,*) rxyz2(1:3)
    write(content2,'(3i2)') rxyz2(1:3)
  case(41)
    read(content,*) nrhead2
    write(content2,'(i10)') nrhead2
  case(42)
    read(content,*) nran2
    write(content2,'(i10)') nran2
  case(43)
    read(content,*) interlace2
    write(content2,*) interlace2
  case(44)
    read(content,*) painter2
    write(content2,'(i2)') painter2
  case(45)
    read(content,*) wincorr2
    write(content2,*) wincorr2
  case(46)
    read(content,*) othernpeff
    write(content2,*) othernpeff
  case(47)
    read(content,*) npeffinput
    write(content2,'(f)') npeffinput
endselect
message(keyid)=trim(reqpara(keyid))//': '//trim(content2)
endsubroutine setpara


subroutine load_para(parafile)
implicit none
character(*)::parafile
integer(4)::eof
character(512)::tmpstr
integer(4)::line
integer(4)::nalphabet
character(128)::key
integer(4)::keyid

call initpara
do keyid=nreqpara+1,npara
  call setpara(keyid,defvalue(keyid))
enddo

write(*,*) 'input parafile file: ',trim(parafile)
write(*,*) '====SET PARAMETERS===='

open(31,file=parafile,form='formatted',status='old')
line=0
do
  tmpstr=''
  ! end of file?
  read(31,'(a512)',iostat=eof) tmpstr 
  if (eof.ne.0) then
    !!!!! write(*,*) 'END of parameter file'
    exit
  endif
  line=line+1
  !!!!! write(*,*) 'line:',line
  !!!!! write(*,*) 'eof:',eof
  ! comment line?
  if (tmpstr(1:1).eq.'#') then
    !!!!! write(*,*) 'comment line'
    !!!!! write(*,*) ''
    cycle
  endif
  ! empty line?
  !!!!! write(*,*) 'line length:',len_trim(adjustl(tmpstr))
  if (len_trim(adjustl(tmpstr)).eq.0) then
    !!!!! write(*,*) 'empty line'
    !!!!! write(*,*) ''
    cycle
  endif

  !!!!! write(*,*) 'content: ',trim(tmpstr)

  nalphabet=index(tmpstr,' ')-1
  key=tmpstr(1:nalphabet)
  !!!!! write(*,*) nalphabet,trim(key)
  keyid= findloc(reqpara,trim(key),1)
  !!!!! write(*,*) 'keyid:',keyid
  paraflag(keyid)=.false.
  tmpstr(1:nalphabet)=' '
  tmpstr=adjustl(tmpstr)
  !!!!! write(*,*) 'new content: ',trim(tmpstr)
  call setpara(keyid,tmpstr)
  write(*,*) trim(message(keyid))

  ! end of processing
enddo
close(31)

do keyid=1,nreqpara
  if (paraflag(keyid)) then
    write(*,*) 'missing parameter: ',trim(reqpara(keyid))
  endif
enddo
if (any(paraflag(1:nreqpara))) then
  stop
endif

write(*,*) '====WHOLE PARAMETER TABLE===='
do keyid=1,npara
  write(*,*) trim(message(keyid))
enddo
write(*,*) '============================='

endsubroutine load_para




endmodule parameters
