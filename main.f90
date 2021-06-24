module fort_test

  type, public :: InnerObj
    real(kind=4), allocatable :: data(:)
  end type

  interface InnerObj
    module procedure :: init__inner_obj
  end interface InnerObj


  type, public :: OuterObj
    character(len=10) :: name
    type(InnerObj), allocatable :: innerObjs(:)
  end type

  interface OuterObj
    module procedure :: init__outer_obj
  end interface OuterObj


  type, public :: MainObj
    type(OuterObj), allocatable :: outerObjs(:)

  contains
    procedure :: add => main_obj__add
  end type

  interface MainObj
    module procedure :: init__main_obj
  end interface MainObj

contains

  type(InnerObj) function init__inner_obj() result(obj)
    obj = InnerObj(null())  ! needed due to gfortran issue
  end function init__inner_obj


  type(OuterObj) function init__outer_obj() result(obj)
    obj = OuterObj("ABCD", null()) ! needed due to gfortran issue
  end function init__outer_obj


  type(MainObj) function init__main_obj() result(obj)
    obj = MainObj(null()) ! needed due to gfortran issue
  end function init__main_obj

  subroutine main_obj__add(self, outer_obj)
    class(MainObj), intent(inout) :: self
    type(OuterObj), intent(in) :: outer_obj

    type(OuterObj), allocatable :: tmp_outer_objs(:)

    allocate(tmp_outer_objs(size(self%outerObjs) + 1))
    tmp_outer_objs(1:size(self%outerObjs)) = self%outerObjs(1:size(self%outerObjs))
    tmp_outer_objs(size(tmp_outer_objs)) = outer_obj

    if (allocated(self%outerObjs)) then
      deallocate(self%outerObjs)
    end if

    call move_alloc(tmp_outer_objs, self%outerObjs)

  end subroutine main_obj__add

end module fort_test

program main
  use fort_test

  type(MainObj) :: main_obj
  type(OuterObj) :: outer_obj
  type(InnerObj) :: inner_obj1, inner_obj2


  inner_obj1 = InnerObj()
  allocate(inner_obj1%data(0))
  inner_obj1%data = [inner_obj1%data, 1.0]
  inner_obj1%data = [inner_obj1%data, 2.0]
  inner_obj1%data = [inner_obj1%data, 3.0]

  inner_obj2 = InnerObj()
  allocate(inner_obj2%data(0))
  inner_obj2%data = [inner_obj2%data, 1.0]
  inner_obj2%data = [inner_obj2%data, 2.0]
  inner_obj2%data = [inner_obj2%data, 3.0]

  outer_obj = OuterObj()
  outer_obj%name = "SOMETHING"
  allocate(outer_obj%innerObjs(0))
  outer_obj%innerObjs = [outer_obj%innerObjs, inner_obj1]
  outer_obj%innerObjs = [outer_obj%innerObjs, inner_obj2]

  main_obj = MainObj()
  call main_obj%add(outer_obj)

end program main