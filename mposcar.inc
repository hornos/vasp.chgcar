  
  type t_poscar
    character(len=32) :: com
    real :: lattc
    real,dimension(3,3) :: lattv
    integer :: ntyp,nion
    character(len=8) :: ptyp
    character(len=2),pointer :: type(:)
    integer,pointer :: nityp(:)
  end type
