program chgcar
  use ifport
  use prec
  use mio
  use mchgcar
  implicit none

  ! input and output units
  integer,dimension(3) :: iou = (/50,51,52/)
  character(len=32),dimension(3) :: ionam
  character(len=32767),dimension(2) :: line
  character(len=32767) :: buf
  real(q),dimension(2) :: c
  real(q),dimension(2,3) :: pos
  real(q),dimension(3,5) :: v
  integer,dimension(2,4) :: ng
  integer :: i,j,ni,lm,lr,lc,ioerr

  ! timing
  real :: totalt
  real,dimension(2) :: telapsed

  ! structures
  type(t_poscar),dimension(2) :: car

  print*,'Warning: Spin density is not implemented!'

  lparam: do i=1,3
    if(i.lt.3) read(*,fmt="(f3.6)"),c(i)
    read(*,fmt="(a)"),ionam(i)
  end do lparam
!
  lopen: do i=1,3
    if(i.eq.3) then
      call wopen(iou(i),ionam(i),ioerr)
    else
      call ropen(iou(i),ionam(i),ioerr)
    end if
    if(ioerr/=0) then 
      print*,"Open error: ",ionam(i),ioerr
      stop
    end if
  end do lopen

! read header
  lrdhdr: do i=1,2
    call chgrdhdr(iou(i),car(i),ioerr)
    if(ioerr/=0) then 
      print*,"Header error: ",ionam(i),ioerr
      stop
    end if
  end do lrdhdr

! check 1
  if(car(1)%nion.ne.car(2)%nion) then
    print*,"Warning: Atom numbers are not equal!",car(1)%nion,car(2)%nion
!    stop
  end if


  print*
! write output header
  call chgwrhdr(iou(3),car(1),ioerr)
  if(ioerr/=0) then 
    print*,"Header error: ",ionam(3),ioerr
    stop
  end if

! dump coords 
  lcoord1: do ni=1,car(1)%nion
    read(iou(1),*,iostat=ioerr) (pos(1,i),i=1,3)
    if(ioerr/=0) stop
    write(unit=iou(3),fmt="(3f12.6)",iostat=ioerr) (pos(1,i),i=1,3)
    if(ioerr/=0) stop
  end do lcoord1

! pad read 2 input
  lcoord2: do ni=1,car(2)%nion
    read(iou(2),*,iostat=ioerr) (pos(2,i),i=1,3)
    if(ioerr/=0) stop
  end do lcoord2


! dump padding
  write(unit=iou(3),fmt="(a)",iostat=ioerr) ''
  if(ioerr/=0) stop

! grid dim
  lng: do i=1,2
    read(iou(i),*,iostat=ioerr) (ng(i,j),j=1,3)
    if(ioerr/=0) stop
    print "(f6.3,x,20a)",c(i),ionam(i)
    print "(3i5)",(ng(i,j),j=1,3)
    print*
  end do lng
  ng(1,4) = 1
  ng(2,4) = 1

! check
  do j=1,3
    if(ng(1,j).ne.ng(2,j)) then
      print*,'Grid dimension error'
      stop
    end if
    do i=1,2
      ng(i,4) = ng(i,4) * ng(i,j)
    end do
  end do

  write(unit=iou(3),fmt="(3i8)",iostat=ioerr) (ng(1,i),i=1,3)
  if(ioerr/=0) stop

! grid data
  lm=floor(ng(1,4)/5.0)
  lr=mod(ng(1,4),5)
  ! print*,ng(1,4),lm*5,lm,lr
  print*,"Processing..."
  lgrid: do lc=1,lm
    call vline(iou,ioerr,c,v,5)
  end do lgrid

  if(lr.gt.0) then
    call vline(iou,ioerr,c,v,lr)
  end if
!
  lclose: do i=1,3
    close(unit=iou(i))
  end do lclose

  totalt=etime(telapsed)
  print "(f12.3,x,6a)",totalt,"sec"

  contains
    subroutine vcomp(c,v,im)
      real(q),dimension(2) :: c
      real(q),dimension(3,5) :: v
      integer :: i,im
      do i=1,im
        v(3,i)=c(1)*v(1,i)+c(2)*v(2,i)
      end do
    end subroutine

    subroutine vline(iou,ioerr,c,v,jm)
      real(q),dimension(2) :: c
      real(q),dimension(3,5) :: v
      integer :: i,j,jm,ioerr
      integer,dimension(3) :: iou
      do i=1,2
        read(iou(i),*,iostat=ioerr) (v(i,j),j=1,jm)
        if(ioerr/=0) then
          print*,"Input error: ",i,ioerr
          stop
        end if
      end do
      call vcomp(c,v,jm)
      write(iou(3),fmt="(5E18.9)",iostat=ioerr) (v(3,j),j=1,jm)
      if(ioerr/=0) then
        print*,"Output error: ",i,ioerr
        stop
      end if
    end subroutine
end program
