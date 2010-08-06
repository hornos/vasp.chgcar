module mchgcar
  implicit none
  include "mposcar.inc"

  public :: chgrdhdr,chgwrhdr
  
  contains
    subroutine chgrdhdr(iou,cc,ioerr)
      ! integer,external :: nitems
      integer :: nitems
      integer :: iou,ioerr,i,j,ni
      integer :: nions=0
      character(len=32767) :: line,buf
      character(len=2) :: ch
      type(t_poscar) :: cc
      
      ! comment
      read(unit=iou,fmt="(a)",iostat=ioerr), cc%com
      if(ioerr/=0) return

      ! lattice constant
      read(iou,*,iostat=ioerr), cc%lattc
      if(ioerr/=0) return

      ! lattice vectors
      read(iou,*,iostat=ioerr) ((cc%lattv(i,j),i=1,3),j=1,3)
      if(ioerr/=0) return
      
      ! species
      read(unit=iou,fmt="(a)",iostat=ioerr), line
      if(ioerr/=0) return
      cc%ntyp=nitems(line,buf,.true.,'a')
      allocate(cc%type(cc%ntyp))
      read(line,*,iostat=ioerr) (cc%type(ni),ni=1,cc%ntyp)
      if(ioerr/=0) return

      ! ions
      read(unit=iou,fmt="(a)",iostat=ioerr), line
      if(ioerr/=0) return
      cc%ntyp=nitems(line,buf,.true.,'a')
      allocate(cc%nityp(cc%ntyp))
      read(line,*,iostat=ioerr) (cc%nityp(ni),ni=1,cc%ntyp)
      if(ioerr/=0) return

      ! pos type
      read(unit=iou,fmt="(a)",iostat=ioerr), line
      if(ioerr/=0) return
      cc%ptyp=trim(line)

      cc%nion = 0
      do i=1,cc%ntyp
        cc%nion = cc%nion + cc%nityp(i)
      end do
    end subroutine chgrdhdr
    
    
    subroutine chgwrhdr(iou,cc,ioerr)
      integer :: iou,ioerr,i,j
      type(t_poscar) :: cc
      write(unit=iou,fmt="(a)",iostat=ioerr), trim(cc%com)
      if(ioerr/=0) return

      write(unit=iou,fmt="(f16.10)",iostat=ioerr), cc%lattc
      if(ioerr/=0) return

      write(unit=iou,fmt="(3f16.10)",iostat=ioerr) ((cc%lattv(i,j),i=1,3),j=1,3)
      if(ioerr/=0) return

      write(unit=iou,fmt="(20a5)",iostat=ioerr) (trim(cc%type(i)),i=1,cc%ntyp)
      if(ioerr/=0) return

      write(unit=iou,fmt="(20i6)",iostat=ioerr) (cc%nityp(i),i=1,cc%ntyp)
      if(ioerr/=0) return

      write(unit=iou,fmt="(a8)",iostat=ioerr), trim(cc%ptyp)
      if(ioerr/=0) return
    end subroutine
end module mchgcar
