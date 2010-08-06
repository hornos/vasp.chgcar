module mposcar
  implicit none
  include "mposcar.inc"

  public :: posrdhdr
  
  contains
    subroutine posrdhdr(iou,pc,ioerr)
      integer :: iou,ioerr
      type(t_poscar) :: pc
    end subroutine posrdhdr
end module mposcar
