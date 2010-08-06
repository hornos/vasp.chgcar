module mio
  implicit none

  public :: ropen,wopen

  contains
    subroutine wopen(iou,ionam,ioerr)
      integer :: iou,ioerr
      character(len=*) :: ionam
      open(unit=iou, file=trim(ionam), & 
     &     action="write", iostat=ioerr)
    end subroutine wopen

    subroutine ropen(iou,ionam,ioerr)
      integer :: iou,ioerr
      character(len=*) :: ionam
      open(unit=iou, file=trim(ionam), & 
     &     status="old", action="read", iostat=ioerr)
    end subroutine ropen

end module mio
